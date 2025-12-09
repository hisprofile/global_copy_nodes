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
from uuid import uuid4
from bpy.types import Operator, Menu, AddonPreferences
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

#prohibits = {
#    bpy.types.NodeSocket: {'

map_og_to_copy = dict()

def recursive_property_setter(original, copy, properties: bpy.types.bpy_prop_collection, surface_scan=False):
    input = print
    if (original in map_og_to_copy) and not surface_scan:
        return
    #elif original in map_og_to_copy:
    #    print(original, 'has already been accessed! but continuing anyways')
    map_og_to_copy[original] = copy
    for prop in properties:
        prop_id = prop.identifier
        #print(prop_id == 'default_value')
        if isinstance(original, bpy.types.NodeSocket) and prop_id in {'links', 'node'}: continue
        if prop_id in {
            'rna_type',
            'original',
            'srna',
            'links',
            'node',
            'display_shape',
            'internal_links',
            #'location_absolute'
            }: continue
        if prop_id.startswith('bl_'): continue
        #input((prop_id, repr(original), repr(copy)))
        if prop.type == 'POINTER':
            if prop.is_readonly:
                #input((prop_id, repr(original), repr(copy), 'is readonly'))
                original_prop = getattr(original, prop_id)
                copy_prop = getattr(copy, prop_id)
                recursive_property_setter(original_prop, copy_prop, prop.fixed_type.properties)
                if isinstance(copy_prop, bpy.types.CurveMapping):
                    copy_prop.update()
            else:
                #input((prop_id, repr(original), repr(copy), 'can be set'))
                pointer_value = getattr(original, prop_id)
                setattr(copy, prop_id, map_og_to_copy.get(pointer_value, pointer_value))
            continue
        if prop.type == 'COLLECTION':
            original_items = getattr(original, prop_id)
            copy_items = getattr(copy, prop_id)
            #print(original, original_items, copy, copy_items)
            prop_srna_type = prop.srna
            #print(original, prop_id, prop)
            if prop_srna_type:
                new_parameters = {param.identifier: getattr(param, 'default_array', None) or param.default
                                  for param in prop_srna_type.functions['new'].parameters
                                  if not param.type in {'POINTER', 'COLLECTION'}}
                #print(new_parameters)
                #print('collection can be added to', new_parameters)
                try:
                    #copy_items.clear()
                    [copy_items.remove(item) for item in copy_items[1:]]
                except:
                    pass
                try:
                    for _ in range(len(original_items) - len(copy_items)):
                        copy_items.new(**new_parameters)
                        #print(x)
                except Exception as e:
                    #raise
                    #print(e)ab
                    pass
                #print('don\'t think collection can be added to')
            #print(prop_fixed_type, list(prop_fixed_type.properties))
            for og_item, copy_item in zip(original_items, copy_items):
                #copy_item = copy_items.get(item.
                #print(og_item, copy_item, 'penis')
                #input((prop_id, repr(original), repr(copy), 'item'))
                recursive_property_setter(og_item, copy_item, og_item.bl_rna.properties)
            
            continue
        if prop.is_readonly:
            #input((prop_id, repr(original), repr(copy), 'bool/str/float/int readonly'))
            continue
        #input((prop_id, repr(original), repr(copy)))
        #print(original, copy, prop_id)
        try:
            setattr(copy, prop_id, getattr(original, prop_id))
        except:
            print(original, prop_id)
        
    pass


def get_center_location_of_nodes(nodes: Iterable[bpy.types.Node]) -> Vector:
    """
    Get the center point of all the nodes
    """
    from math import inf
    minimum = Vector((inf, inf))
    maximum = Vector((-inf, -inf))

    for node in nodes:
        if node.parent: continue
        print(node.dimensions)
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

class node_OT_global_clipboard_copy(Operator):
    bl_idname = 'node.global_clipboard_copy'
    bl_label = 'Global Copy Nodes'
    bl_description = 'Copy the nodes globally to paste into another project'

    @classmethod
    def poll(cls, context):
        return context.area.type == 'NODE_EDITOR'

    def execute(self, context):
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
        selected_nodes = set(node for node in node_tree.nodes if node.select)# + set(node.parent for node in node_tree.nodes if node.select)
        parent_nodes = set(node.parent for node in selected_nodes)
        parent_nodes.discard(None)
        selected_nodes.update(parent_nodes)
        #print(selected_nodes, parent_nodes)
        #(selected_nodes.add(node.parent) for node in set(selected_nodes))
        #selected_nodes.discard(None)
        selected_nodes = list(selected_nodes)
        #[selected_nodes.add(node.parent) for node in set(selected_nodes)]
        copy_counterparts = list()

        if not selected_nodes: return {'CANCELLED'}

        if blend_data.node_groups.get(NODE_GROUP_NAME):
            blend_data.node_groups.remove(blend_data.node_groups[NODE_GROUP_NAME])

        node_group = blend_data.node_groups.new(NODE_GROUP_NAME, tree_identifier)
        node_group.use_fake_user = True

        # we will always keep the pasted nodes, so lets compare uuid4 tags against latest & current (if pasted before) to check for an update
        unique_id = str(uuid4())
        node_group['global_node_copy_unique_id'] = unique_id
        #context.window_manager['global_node_copy_unique_id'] = unique_id
        #print(map_og_to_copy)
        for node in selected_nodes:
            new_node = node_group.nodes.new(node.bl_idname)
            if hasattr(node, 'node_tree'):
                new_node.node_tree = node.node_tree
            copy_counterparts.append(new_node)
            map_og_to_copy[node] = new_node

        for node, new_node in zip(selected_nodes, copy_counterparts):
            setattr(new_node, 'parent', map_og_to_copy.get(node.parent, None))
            recursive_property_setter(node, new_node, node.bl_rna.properties, True)
            #recursive_property_setter(node, new_node, node.bl_rna.properties)
            #print(map_og_to_copy)
            #node_group.update()

        for node in list(selected_nodes):
            counter_part = map_og_to_copy[node]
            for inp1, inp2 in zip(node.inputs, counter_part.inputs):
                map_og_to_copy[inp1] = inp2
            for inp1, inp2 in zip(node.outputs, counter_part.outputs):
                map_og_to_copy[inp1] = inp2
        #[print(repr(key), repr(value), 'gay') for key, value in map_og_to_copy.items()]
        for link in node_tree.links:
            if not ((link.from_node in selected_nodes) and (link.to_node in selected_nodes)):
                continue
            #print(repr(link.from_socket), repr(link.to_socket))
            #print(map_og_to_copy.get(link.from_node))
            #print(map_og_to_copy.get(link.to_node))
            from_socket = map_og_to_copy[link.from_socket]
            to_socket = map_og_to_copy[link.to_socket]
            #print(repr(from_socket), repr(to_socket))
            #print(dir(link))
            #print(link.is_valid, link.multi_input_sort_id)
            try:
                node_group.links.new(from_socket, to_socket)
            except:
                #print(from_socket, to_socket, link.from_node, link.to_node, link.from_socket, link.to_socket)
                #raise
                continue
        map_og_to_copy.clear()
        return {'FINISHED'}
        #[print(repr(key), repr(value), 'gay') for key, value in map_og_to_copy.items()]

        map_og_to_copy.clear()

        blend_data.libraries.write(node_copy_buffer_path, {node_group})#, path_remap='RELATIVE_ALL', compress=True

        with open(last_path_data, 'w+') as file:
            file.write(blend_data.filepath + '\n' + unique_id)

        #blend_data.node_groups.remove(node_group)

        # copy internally too, so we don't have to use the copy buffer for the same file if someone uses this operator instead of the original
        with context.temp_override(window=context.window, area=context.area, region=context.region):
            bpy.ops.node.clipboard_copy()

        self.report({'INFO'}, 'Nodes copied globally & internally')

        return {'FINISHED'}

def center_nodes_on_timer():
    node_tree = node_tree_to_center
    nodes = [node for node in node_tree.nodes if node.select]

    middle_pos = get_center_location_of_nodes(nodes)

    for node in nodes:
        print(node.dimensions)
        if node.parent: continue
        node.location += mouse_pos - middle_pos

    return None

class node_OT_global_clipboard_paste(Operator):
    bl_idname = 'node.global_clipboard_paste'
    bl_label = 'Global Paste Nodes'
    bl_description = 'Paste nodes that were copied from another file'

    #mouse_pos: Vector = None

    @classmethod
    def poll(cls, context):
        return context.area.type == 'NODE_EDITOR'
    
    def invoke(self, context, event):
        global mouse_pos
        x, y = context.region.view2d.region_to_view(event.mouse_region_x, event.mouse_region_y)
        mouse_pos = Vector((x, y))
        return self.execute(context)

    def execute(self, context):
        global node_tree_to_center

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
                last_copied_path, unique_id = file.read().split('\n')
            
            # if True, we have the most recent copy, but we should prioritize internal copy buffer. as a fallback, use the existing copy buffer, as it exists
            # if False, we do not have the most recent copy. delete the copy buffer node group if it exists, then load the latest
            if unique_id == current_unique_id:
                print('unique id matches current!', unique_id)
                try:
                    assert tree_identifier == existing_node_buffer.bl_idname
                except AssertionError:
                    self.report({'ERROR'}, 'Copied node tree type does not match the active node tree!')
                    return {'CANCELLED'}
                
                #try:
                #    with context.temp_override(window=context.window, area=context.area, region=context.region):
                #        bpy.ops.node.clipboard_paste()
                #    return {'FINISHED'}
                #except:
                #    # the internal copy buffer does not yet exist, but we can still rely on the current copy, as it has not been deleted! 
                #    pass
                copied_node_tree = existing_node_buffer

            else:
                print('unique id does not match current!', unique_id, current_unique_id)
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
        print('PASTING GLOBALLY')

        copied_node_tree.use_fake_user = True

        if copied_node_tree.bl_idname != tree_identifier:
            self.report({'ERROR'}, 'Copied node tree type does not match active node tree!')
            blend_data.node_groups.remove(copied_node_tree)
            return {'CANCELLED'}
        
        new_nodes = set()

        for node in copied_node_tree.nodes:
            new_node = node_tree.nodes.new(node.bl_idname)
            new_nodes.add(new_node)
            if hasattr(node, 'node_tree'):
                new_node.node_tree = node.node_tree
            recursive_property_setter(node, new_node, node.bl_rna.properties)

        for link in copied_node_tree.links:
            from_socket = map_og_to_copy[link.from_socket]
            to_socket = map_og_to_copy[link.to_socket]
            node_tree.links.new(from_socket, to_socket)

        [setattr(node, 'select', False) for node in node_tree.nodes]
        [setattr(node, 'select', True) for node in new_nodes]

        bpy.app.timers.register(center_nodes_on_timer, first_interval=0.0001)

        #middle_pos = get_center_location_of_nodes(new_nodes)
#
        #for node in new_nodes:
        #    if node.parent: continue
        #    node.location += self.mouse_pos - middle_pos

        #with context.temp_override(window=context.window, area=context.area, region=context.region):
        #    bpy.ops.node.clipboard_copy()

        map_og_to_copy.clear()

        if not preferences.link_to_scene:
            return {'FINISHED'}
        
        from bpy_extras import id_map_utils

        ref_map = id_map_utils.get_id_reference_map()
        referenced_ids: set[bpy.types.ID] = id_map_utils.get_all_referenced_ids(copied_node_tree, ref_map)

        objs: set[bpy.types.Object] = set(filter(lambda a: isinstance(a, bpy.types.Object), referenced_ids))
        collections: set[bpy.types.Collection] = set(filter(lambda a: isinstance(a, bpy.types.Collection), referenced_ids))

        [collections.difference_update(set(col.children_recursive)) for col in list(collections)] # get only the top collections
        [objs.difference_update(set(col.all_objects)) for col in collections] # get objects that are not a part of any attached collection
        collections.difference_update(set(context.scene.collection.children_recursive))
        objs.difference_update(set(context.scene.objects))

        if objs or collections:
            link_to_collection = context.scene.collection
            if preferences.create_collections:
                if not (link_to_collection := context.scene.get('global_copy_nodes_default_collection')):
                    link_to_collection = blend_data.collections.new(DEFAULT_COLLECTION_NAME)
                    context.scene['global_copy_nodes_default_collection'] = link_to_collection
                    context.scene.collection.children.link(link_to_collection)
            [link_to_collection.children.link(col) for col in collections]
            [link_to_collection.objects.link(obj) for obj in objs]
            
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

    use_custom_copy_path: BoolProperty(name='Use Custom Copy Path', description='Use a custom path to store the node copy buffer, making it easier to sync copies across different versions')
    custom_copy_path: StringProperty(name='Custom Copy Path', description='The custom path to store node copy buffers, making it easier to sync copies across different versions', subtype='DIR_PATH')
    link_to_scene: BoolProperty(name='Automatically Link To Scene', description='Link attached objects or collections to the scene', default=True)
    create_collections: BoolProperty(name='Create Collections For Attached Objects', description='Create a collection to assign attached objects/collections to, for organization purposes', default=True)

    def draw(self, context):
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

        wm = context.window_manager
        kc = wm.keyconfigs.addon
        if kc:
            layout.label(text="Custom Keymap Assignments:")
            for km, kmi in addon_keymaps:
                draw_kmi([], kc, km, kmi, layout, 0)

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

    kmi_global_copy = km.keymap_items.new('node.global_clipboard_copy', 'C', 'PRESS', shift=True, ctrl=True, repeat=False)
    addon_keymaps.append((km, kmi_global_copy))

    kmi_global_paste = km.keymap_items.new('node.global_clipboard_paste', 'V', 'PRESS', shift=True, ctrl=True, repeat=False)
    addon_keymaps.append((km, kmi_global_paste))

def unregister_keymaps():
    wm = bpy.context.window_manager
    for km, kmi in addon_keymaps:
        km.keymap_items.remove(kmi)
    addon_keymaps.clear()

def register():
    r()
    register_keymaps()
    print('REGISTERING')

def unregister():
    ur()
    unregister_keymaps()
    print('UNREGISTERING')