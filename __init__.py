bl_info = {
    "name" : "Global Copy Nodes",
    "description" : "Copy nodes across .blend projects",
    "author" : "hisanimations",
    "version" : (1, 0, 0),
    "blender" : (3, 5, 0),
    "location" : "Node Editor > Global Copy Nodes",
    "support" : "COMMUNITY",
    "category" : "Porting",
    #"doc_url": "https://github.com/hisprofile/TF2-Trifecta/blob/main/README.md"
}

import bpy
import time
import os
import traceback
from uuid import uuid4
from bpy.types import Operator, AddonPreferences
from bpy.props import StringProperty, BoolProperty
from rna_keymap_ui import draw_kmi
from mathutils import Vector
from typing import Iterable

BASE_PACKAGE = __package__

try:
    assert bpy.app.version >= (4, 2, 0)
    WRITE_PATH = bpy.utils.extension_path_user(BASE_PACKAGE, create=True)
except:
    WRITE_PATH = os.path.dirname(__file__)

NODE_GROUP_NAME = 'global_copy_nodes_buffer'
DEFAULT_COLLECTION_NAME = 'Node Copy Dependencies'
DEFAULT_COLLECTION_PROP_NAME = 'global_copy_nodes_default_collection'
UNIQUE_ID_PROP_NAME = 'global_node_copy_unique_id'
mouse_pos: Vector = None
node_tree_to_center: bpy.types.NodeTree = None

iter_links = None

map_og_to_copy = dict()

def recursive_property_setter(op: bpy.types.Operator, original: bpy.types.bpy_struct, copy: bpy.types.bpy_struct, properties: bpy.types.bpy_prop_collection, surface_scan: bool = False) -> None:
    '''
    Get attributes from the original struct, set them onto copy struct
    
    :param op: Operator that was called
    :type op: bpy.types.Operator
    :param original: The reference bpy_struct
    :type original: bpy.types.bpy_struct
    :param copy: The target bpy_struct
    :type copy: bpy.types.bpy_struct
    :param properties: bl_rna.properties of the original struct
    :type properties: bpy.types.bpy_prop_collection
    :param surface_scan: If False, it will proceed with the scan despite the original already being in the map dict.
    :type surface_scan: bool
    '''
    if (original in map_og_to_copy) and not surface_scan:
        return
    #elif original in map_og_to_copy:
    #    print(original, 'has already been accessed! but continuing anyways')
    map_og_to_copy[original] = copy
    # make inputs the last attribute to access
    if isinstance(original, bpy.types.Node):
        properties = sorted(properties, key=lambda a: a.identifier == 'inputs')
        
    for prop in properties:
        prop_id = prop.identifier
        #if isinstance(original, bpy.types.NodeSocket) and prop_id in {'links', 'node'}: continue
        if prop_id in {
            'rna_type',
            'original',
            'srna',
            'links',
            'node',
            'display_shape',
            'internal_links',
            'location_absolute'
            }: continue
        if prop_id.startswith('bl_'): continue
        
        if prop.type == 'POINTER':
            if prop.is_readonly:
                original_prop = getattr(original, prop_id)
                copy_prop = getattr(copy, prop_id)

                recursive_property_setter(op, original_prop, copy_prop, prop.fixed_type.properties, False)

                # i don't like unique handling cases, but this seems to be necessary. i don't think it's too disruptive to implement anyways
                if isinstance(copy_prop, bpy.types.CurveMapping):
                    copy_prop.update()
            else:
                pointer_value = getattr(original, prop_id)
                setattr(copy, prop_id, map_og_to_copy.get(pointer_value, pointer_value))

        elif prop.type == 'COLLECTION':
            original_items = getattr(original, prop_id)
            copy_items = getattr(copy, prop_id)
            prop_srna_type = prop.srna

            if prop_srna_type:
                new_parameters = {param.identifier: getattr(param, 'default_array', None) or param.default
                                  for param in prop_srna_type.functions['new'].parameters
                                  if not param.type in {'POINTER', 'COLLECTION'}}
                
                # remove as many elements as possible
                try:
                    [copy_items.remove(item) for item in copy_items[1:]]
                except:
                    pass
                
                # add enough elements to match original's count
                try:
                    for _ in range(len(original_items) - len(copy_items)):
                        copy_items.new(**new_parameters)
                except Exception as e:
                    pass

            for og_item, copy_item in zip(original_items, copy_items):
                recursive_property_setter(op, og_item, copy_item, og_item.bl_rna.properties, False)
            
        else:
            if prop.is_readonly:
                continue
            try:
                setattr(copy, prop_id, getattr(original, prop_id))
            except Exception as e:
                #op.setter_fail_count += 1
                traceback.print_exc()
                #op.report({'ERROR'}, 'Could not set')
                print(f'GCN: Could not set path {repr(copy)}, {prop_id} of {type(copy)}, {type(prop)} with value {getattr(original, prop_id)}! Report to developer!\n')


def get_center_location_of_nodes(nodes: Iterable[bpy.types.Node]) -> Vector:
    '''
    Get the middle point of all the nodes.  
    It might be good to run on a timer, letting the nodes draw and get their dimensions property
    
    :param nodes: List of nodes to find the center of
    :type nodes: Iterable[bpy.types.Node]
    :return: Center point of all the nodes
    :rtype: Vector
    '''
    from math import inf
    minimum = Vector((inf, inf))
    maximum = Vector((-inf, -inf))

    for node in nodes:
        if node.parent: continue
        loc = node.location
        width, height = node.dimensions[0], node.dimensions[1]
        corners = [
            loc,
            loc + Vector((width, 0)),
            loc + Vector((width, -height)),
            loc + Vector((0, -height))
        ]
        for corner in corners:
            minimum = Vector(map(min, corner, minimum))
            maximum = Vector(map(max, corner, maximum))

    middle = (minimum + maximum) / 2

    return middle


def center_nodes_on_timer():
    '''
    Register this function on a timer with a small interval, letting the nodes draw.
    Access the node tree to modify using the global variable "node_tree_to_center", and use the global variable "mouse_pos" to know where to center to
    '''
    node_tree = node_tree_to_center
    nodes = [node for node in node_tree.nodes if node.select]

    middle_pos = get_center_location_of_nodes(nodes)

    for node in nodes:
        if node.parent: continue
        node.location += mouse_pos - middle_pos

    return None

def copy_nodes_to_node_tree(op: bpy.types.Operator, src_node_tree: bpy.types.NodeTree, dst_node_tree: bpy.types.NodeTree, src_nodes: list[bpy.types.Node]) -> list[bpy.types.Node]:
    '''
    Recursively copy nodes from one node tree into another, using bl_rna properties and a list of source nodes
    
    :param op: Operator that was called
    :type op: bpy.types.Operator
    :param src_node_tree: Source Node Tree
    :type src_node_tree: bpy.types.NodeTree
    :param dst_node_tree: Destination Node Tree
    :type dst_node_tree: bpy.types.NodeTree
    :param src_nodes: Nodes to copy from
    :type src_nodes: list[bpy.types.Node]
    :return: The new copied nodes
    :rtype: list[Node]
    '''
    dst_nodes: list[bpy.types.Node] = list()
        
    # 1.
    # make copies of nodes, and map the original to the copies
    for node in src_nodes:
        new_node = dst_node_tree.nodes.new(node.bl_idname)
        if hasattr(node, 'node_tree'):
            new_node.node_tree = node.node_tree
        dst_nodes.append(new_node)
        map_og_to_copy[node] = new_node

    # 2.
    # recursively set the properties of the copied nodes, using the original nodes as a reference
    # set the parent to make positioning more convenient
    for node, new_node in zip(src_nodes, dst_nodes):
        setattr(new_node, 'parent', map_og_to_copy.get(node.parent, None))
        recursive_property_setter(op, node, new_node, node.bl_rna.properties, True)

    # 3.
    # update the mapping for original inputs/outputs to the copied inputs/outputs
    # it seems this should be done after properties are set. i've found odd cases where the socket data is seemingly offset in memory. maybe because of dynamic sockets?
    # in the mapping dictionary, it lead to an output referencing itself. no idea how that works, but we want to make sure we have the most updated references
    for node in list(src_nodes):
        counter_part = map_og_to_copy[node]
        for inp1, inp2 in zip(node.inputs, counter_part.inputs):
            map_og_to_copy[inp1] = inp2
        for out1, out2 in zip(node.outputs, counter_part.outputs):
            map_og_to_copy[out1] = out2

    # 4.
    # make the links!
    for link in src_node_tree.links:
        if not ((link.from_node in src_nodes) and (link.to_node in src_nodes)):
            continue
        
        from_socket = map_og_to_copy[link.from_socket]
        to_socket = map_og_to_copy[link.to_socket]
        dst_node_tree.links.new(from_socket, to_socket)

    return dst_nodes


class node_OT_global_clipboard_copy(Operator):
    bl_idname = 'node.global_clipboard_copy'
    bl_label = 'Global Copy Nodes'
    bl_description = 'Copy the nodes globally to paste into another project'

    setter_fail_count = 0

    @classmethod
    def poll(cls, context):
        return context.area.type == 'NODE_EDITOR'

    def execute(self, context):
        self.setter_fail_count = 0
        preferences = context.preferences.addons[BASE_PACKAGE].preferences
        if preferences.use_custom_copy_path:
            if not os.path.exists(preferences.custom_copy_path):
                self.report({'ERROR'}, 'The add-on\'s "Custom Copy Path" does not exist! Set it correctly in preferences!')
                return {'CANCELLED'}
            node_copy_buffer_path = os.path.join(preferences.custom_copy_path, 'node_copy_buffer.blend')
            last_path_data = os.path.join(preferences.custom_copy_path, 'last_path_data')
        else:
            node_copy_buffer_path = os.path.join(WRITE_PATH, 'node_copy_buffer.blend')
            last_path_data = os.path.join(WRITE_PATH, 'last_path_data')

        map_og_to_copy.clear()

        blend_data = context.blend_data
        node_tree = context.space_data.node_tree

        if node_tree == None: return {'CANCELLED'}

        tree_identifier = node_tree.bl_idname

        selected_nodes = set(node for node in node_tree.nodes if node.select)
        if not selected_nodes: return {'CANCELLED'}

        parent_nodes = set(node.parent for node in selected_nodes)
        selected_nodes.update(parent_nodes)
        selected_nodes.discard(None)
        del parent_nodes

        selected_nodes = list(selected_nodes)

        if blend_data.node_groups.get(NODE_GROUP_NAME):
            blend_data.node_groups.remove(blend_data.node_groups[NODE_GROUP_NAME])

        # we will always keep the pasted nodes, so lets compare uuid4 tags against latest & current (if pasted before) to check for an update
        unique_id = str(uuid4())
        node_group = blend_data.node_groups.new(NODE_GROUP_NAME, tree_identifier)
        node_group.use_fake_user = True
        node_group['global_node_copy_unique_id'] = unique_id

        copy_nodes_to_node_tree(self, node_tree, node_group, selected_nodes)

        if self.setter_fail_count:
            self.report({'ERROR'}, f'Failed to set {self.setter_fail_count} value(s). Open console and report errors to developer!')

        map_og_to_copy.clear()

        blend_data.libraries.write(node_copy_buffer_path, {node_group})#, path_remap='RELATIVE_ALL', compress=True

        with open(last_path_data, 'w+') as file:
            file.write('\n'.join([blend_data.filepath, unique_id, tree_identifier]))

        # copy internally too
        if preferences.copy_to_internal_buffer:
            with context.temp_override(window=context.window, area=context.area, region=context.region):
                bpy.ops.node.clipboard_copy()
            self.report({'INFO'}, 'Nodes copied globally & internally')
        else:
            self.report({'INFO'}, 'Nodes copied globally')

        return {'FINISHED'}
    

class node_OT_global_clipboard_paste(Operator):
    bl_idname = 'node.global_clipboard_paste'
    bl_label = 'Global Paste Nodes'
    bl_description = 'Paste nodes that were copied from another file'

    setter_fail_count = 0

    move_modal: BoolProperty(name='Move Modal', default=False)
    last_pos: Vector = None
    original_offset: Vector = None

    bl_options = {'UNDO'}

    @classmethod
    def poll(cls, context):
        return context.area.type == 'NODE_EDITOR'
    
    def get_mouse_in_region(self, context: bpy.types.Context, pos: Iterable):
        x, y = context.region.view2d.region_to_view(pos[0], pos[1])
        return Vector((x, y))

    def invoke(self, context, event):
        global mouse_pos
        mouse_pos = self.get_mouse_in_region(context, [event.mouse_region_x, event.mouse_region_y])
        self.original_offset = mouse_pos.copy()
        self.last_pos = self.original_offset.copy()
        return self.execute(context)
    
    def modal(self, context, event):
        context.window.cursor_modal_set('SCROLL_XY')
        node_tree = context.space_data.node_tree
        nodes = [node for node in node_tree.nodes if node.select]

        if event.type == 'MOUSEMOVE':
            new_pos = self.get_mouse_in_region(context, [event.mouse_region_x, event.mouse_region_y])
            offset = new_pos - self.last_pos
            for node in nodes:
                if node.parent: continue
                node.location += offset
            self.last_pos = new_pos

        if event.type in {'RIGHTMOUSE', 'ESC'}:
            offset = self.original_offset - self.last_pos
            for node in nodes:
                if node.parent: continue
                node.location += offset
            self.move_modal = False
            context.window.cursor_modal_set('DEFAULT')
            return {'FINISHED'}
        
        if event.type in {'LEFTMOUSE', 'RETURN'}:
            self.move_modal = False
            context.window.cursor_modal_set('DEFAULT')
            return {'FINISHED'}
        
        return {'RUNNING_MODAL'}

    def execute(self, context):
        global node_tree_to_center

        self.setter_fail_count = 0
        preferences = context.preferences.addons[BASE_PACKAGE].preferences
        if preferences.use_custom_copy_path:
            if not os.path.exists(preferences.custom_copy_path):
                self.report({'ERROR'}, 'The add-on\'s "Custom Copy Path" does not exist! Set it correctly in preferences!')
                return {'CANCELLED'}
            node_copy_buffer_path = os.path.join(preferences.custom_copy_path, 'node_copy_buffer.blend')
            last_path_data = os.path.join(preferences.custom_copy_path, 'last_path_data')
        else:
            node_copy_buffer_path = os.path.join(WRITE_PATH, 'node_copy_buffer.blend')
            last_path_data = os.path.join(WRITE_PATH, 'last_path_data')

        map_og_to_copy.clear()
        blend_data = context.blend_data
        node_tree = context.space_data.node_tree
        if node_tree == None: return {'CANCELLED'}

        node_tree_to_center = node_tree
        tree_identifier = node_tree.bl_idname

        existing_node_buffer = blend_data.node_groups.get(NODE_GROUP_NAME, dict())
        current_unique_id = existing_node_buffer.get('global_node_copy_unique_id', '')

        if os.path.exists(last_path_data):
            with open(last_path_data, 'r') as file:
                last_copied_path, unique_id, buffer_tree_identifier = file.read().split('\n')

            try:
                assert buffer_tree_identifier == tree_identifier
            except AssertionError:
                self.report({'ERROR'}, 'Copied node tree type does not match the active node tree!')
                return {'CANCELLED'}
            
            # if True, we have the most recent copy, but we should prioritize internal copy buffer. as a fallback, use the existing copy buffer, as it exists
            # update: it might be better to separate the global and internal copy buffers, letting users make the most of either. regardless, the global copy buffer can behave the same as the internal, as it won't duplicate data
            # if False, we do not have the most recent copy. delete the copy buffer node group if it exists, then load the latest
            if unique_id == current_unique_id:
                #print('unique id matches current!', unique_id)

                #try:
                #    with context.temp_override(window=context.window, area=context.area, region=context.region):
                #        bpy.ops.node.clipboard_paste()
                #    return {'FINISHED'}
                #except:
                #    # the internal copy buffer does not yet exist, but we can still rely on the current copy, as it has not been deleted! 
                #    pass

                copied_node_tree = existing_node_buffer

            else:
                #print('unique id does not match current!', unique_id, current_unique_id)
                if blend_data.node_groups.get(NODE_GROUP_NAME):
                    blend_data.node_groups.remove(blend_data.node_groups[NODE_GROUP_NAME])
                if bpy.app.version >= (5, 0, 0):
                    with blend_data.libraries.load(node_copy_buffer_path, reuse_local_id=True) as (src, dst):
                        dst.node_groups = [NODE_GROUP_NAME]
                else:
                    with blend_data.libraries.load(node_copy_buffer_path) as (src, dst):
                        dst.node_groups = [NODE_GROUP_NAME]
                copied_node_tree = dst.node_groups[0]
        else:
            self.report({'ERROR'}, 'The global clipboard is empty')
            return {'CANCELLED'}

        copied_node_tree.use_fake_user = True

        new_nodes = copy_nodes_to_node_tree(self, copied_node_tree, node_tree, list(copied_node_tree.nodes))

        if self.setter_fail_count:
            self.report({'ERROR'}, f'Failed to set {self.setter_fail_count} value(s). Open console and report errors to developer!')

        [setattr(node, 'select', False) for node in node_tree.nodes]
        [setattr(node, 'select', True) for node in new_nodes]

        #middle_pos = get_center_location_of_nodes(new_nodes)

        #for node in new_nodes:
        #    if node.parent: continue
        #    node.location += self.mouse_pos - middle_pos

        #with context.temp_override(window=context.window, area=context.area, region=context.region):
        #    bpy.ops.node.clipboard_copy()

        # center nodes on a timer. should we save dimension data in the copy buffer data text file?
        bpy.app.timers.register(center_nodes_on_timer, first_interval=0.0001)

        map_og_to_copy.clear()

        if not preferences.link_to_scene:
            if self.move_modal:
                context.window_manager.modal_handler_add(self)
                return {'RUNNING_MODAL'}
            return {'FINISHED'}
        
        from bpy_extras import id_map_utils

        # 1.
        # get all IDs associated with the imported nodes
        ref_map: dict[bpy.types.ID, set[bpy.types.ID]] = id_map_utils.get_id_reference_map()
        referenced_ids: set[bpy.types.ID] = id_map_utils.get_all_referenced_ids(copied_node_tree, ref_map)

        # 2.
        # filter objects and collections
        objs: set[bpy.types.Object] = set(filter(lambda a: isinstance(a, bpy.types.Object), referenced_ids))
        collections: set[bpy.types.Collection] = set(filter(lambda a: isinstance(a, bpy.types.Collection), referenced_ids))

        # 3.
        # more filtering
        [collections.difference_update(set(col.children_recursive)) for col in list(collections)] # get only the top collections
        [objs.difference_update(set(col.all_objects)) for col in collections] # get objects that are not a part of any attached collection
        collections.difference_update(set(context.scene.collection.children_recursive)) # remove collections that are associated with the active scene (if pasted more than once from the same copy)
        objs.difference_update(set(context.scene.objects)) # remove objects that are associated with the active scene (if pasted more than once from the same copy)

        # 4.
        # link to scene if there are any valid objects or collections
        if objs or collections:
            link_to_collection = context.scene.collection
            if preferences.create_collections:
                if not (link_to_collection := context.scene.get('global_copy_nodes_default_collection')):
                    link_to_collection = blend_data.collections.new(DEFAULT_COLLECTION_NAME)
                    context.scene['global_copy_nodes_default_collection'] = link_to_collection
                    context.scene.collection.children.link(link_to_collection)

            [link_to_collection.children.link(col) for col in collections]
            [link_to_collection.objects.link(obj) for obj in objs]
        
        if self.move_modal:
            context.window_manager.modal_handler_add(self)
            return {'RUNNING_MODAL'}
        
        return {'FINISHED'}


class global_copy_nodes_OT_temp_path_setter(Operator):
    bl_idname = 'global_copy_nodes.temp_path_setter'
    bl_label = 'Set to Temp Folder'
    bl_description = 'Set the custom copy path to the temporary folder directory, where Blender keeps backups'

    def execute(self, context):
        from pathlib import Path
        preferences = context.preferences.addons[BASE_PACKAGE].preferences
        preferences.custom_copy_path = str(Path(bpy.app.tempdir).parent)
        return {'FINISHED'}

class addon_preferences(AddonPreferences):
    bl_idname = BASE_PACKAGE

    use_custom_copy_path: BoolProperty(
        name='Use Custom Copy Path', 
        description='Use a custom path to store the node copy buffer, making it easier to sync copies across different versions',
        default=False
    )
    custom_copy_path: StringProperty(
        name='Custom Copy Path',
        description='The custom path to store node copy buffers, making it easier to sync copies across different versions',
        subtype='DIR_PATH'
    )
    link_to_scene: BoolProperty(
        name='Automatically Link To Scene',
        description='Link attached objects or collections to the scene',
        default=True
    )
    create_collections: BoolProperty(
        name='Create Collections For Attached Objects',
        description='Create a collection to assign attached objects/collections to, for organization purposes',
        default=True
    )
    copy_to_internal_buffer: BoolProperty(
        name='Copy To Internal Buffer',
        description='If True, the add-on will copy to both the global and internal buffer. If False, then only the global buffer',
        default=True
    )

    def draw(self, context: bpy.types.Context):
        layout = self.layout
        col = layout.column(align=False)
        
        col.label(text='Custom Copy Path')
        box = col.box()
        box.prop(self, 'use_custom_copy_path')

        main = box.row(align=False)
        main.enabled = self.use_custom_copy_path

        row = main.row(align=True)
        row.alignment = 'LEFT'
        row.label(text='Custom Copy Path:')
        
        row = main.row()
        row.alignment = 'EXPAND'
        row.prop(self, 'custom_copy_path', text='')
        
        row = main.row()
        row.alignment = 'RIGHT'
        row.operator('global_copy_nodes.temp_path_setter', icon='FILEBROWSER')

        col = layout.column(align=False)
        col.label(text='Link to Scene')
        box = col.box()
        
        row = box.row()
        row.prop(self, 'link_to_scene')
        
        row = box.row()
        row.enabled = self.link_to_scene
        row.prop(self, 'create_collections')

        col = layout.column()
        col.prop(self, 'copy_to_internal_buffer')

        wm = context.window_manager
        kc = wm.keyconfigs.user
        km: bpy.types.KeyMap = kc.keymaps.get('Node Editor')
        if kc and km:
            layout.label(text="Custom Keymap Assignments:")
            for operator in [
                node_OT_global_clipboard_copy,
                node_OT_global_clipboard_paste
            ]:
                
                if not (kmi := km.keymap_items.get(operator.bl_idname)):
                    continue
                draw_kmi([], kc, km, kmi, layout, 0)

def draw_operators(self, context):
    self.layout.separator()
    self.layout.operator(node_OT_global_clipboard_copy.bl_idname, icon='COPYDOWN')
    self.layout.operator(node_OT_global_clipboard_paste.bl_idname, icon='PASTEDOWN').move_modal = True

classes = [
    node_OT_global_clipboard_copy,
    node_OT_global_clipboard_paste,
    global_copy_nodes_OT_temp_path_setter,
    addon_preferences
]


r, ur = bpy.utils.register_classes_factory(classes)

addon_keymaps = []

def register_keymaps():
    wm = bpy.context.window_manager
    kc = wm.keyconfigs.addon
    km = kc.keymaps.new(name='Node Editor', space_type='NODE_EDITOR', region_type='WINDOW')
    kmis = km.keymap_items

    if not (kmi := kmis.get(node_OT_global_clipboard_copy.bl_idname)):
        kmi = km.keymap_items.new(node_OT_global_clipboard_copy.bl_idname, 'C', 'PRESS', shift=True, ctrl=True, repeat=False)
    else:
        kmi.idname = node_OT_global_clipboard_copy.bl_idname
    addon_keymaps.append((km, kmi))

    if not (kmi := kmis.get(node_OT_global_clipboard_paste.bl_idname)):
        kmi = km.keymap_items.new(node_OT_global_clipboard_paste.bl_idname, 'V', 'PRESS', shift=True, ctrl=True, repeat=False)
    else:
        kmi.idname = node_OT_global_clipboard_paste.bl_idname
    addon_keymaps.append((km, kmi))

def unregister_keymaps():
    wm = bpy.context.window_manager
    for km, kmi in addon_keymaps:
        km.keymap_items.remove(kmi)
    addon_keymaps.clear()

def register():
    r()
    bpy.types.NODE_MT_node.append(draw_operators)
    register_keymaps()

def unregister():
    ur()
    bpy.types.NODE_MT_node.remove(draw_operators)
    unregister_keymaps()