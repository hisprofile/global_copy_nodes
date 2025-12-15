bl_info = {
    "name" : "Global Copy Nodes",
    "description" : "Copy nodes across .blend projects",
    "author" : "hisanimations",
    "version" : (1, 0, 5),
    "blender" : (3, 5, 0),
    "location" : "Node Editor > Global Copy Nodes",
    "support" : "COMMUNITY",
    "category" : "Node",
    "doc_url": "https://github.com/hisprofile/global_copy_nodes/blob/main/README.md"
}

import bpy
import os
import traceback
import numpy as np
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
BUFFER_NAME = 'global_copy_nodes_buffer_info'
BUFFER_BLEND = 'node_copy_buffer.blend'

map_og_to_copy: dict[bpy.types.bpy_struct, bpy.types.bpy_struct] = dict()

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
    
    map_og_to_copy[original] = copy

    # make inputs and outputs the last attribute to access
    if isinstance(original, bpy.types.Node):
        properties = sorted(properties, key=lambda a: a.identifier in {'inputs', 'outputs'})
        
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
            'location_absolute',
            'node_tree'
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
                pointer_value = map_og_to_copy.get(pointer_value, pointer_value) # get the corresponding value if possible
                pointer_fixed_type = type(prop.fixed_type)

                if not isinstance(pointer_value, pointer_fixed_type):
                    continue
                    # this seems to happen with the Field to Grid node.
                    # the node's "active_item" attribute expects a bpy.types.RepeatItem type, yet when accessed, 
                    # it's clearly grabbing from the node's "grid_items" collection of bpy.types.GeometryNodeFieldToGridItem

                    # this is the first time i've seen a property return a different value type than what it's expected to have.
                    # so as a check, just make sure that the value we want to set with matches the type the pointer expects. otherwise, exception

                setattr(copy, prop_id, pointer_value)


        elif prop.type == 'COLLECTION':
            original_items = getattr(original, prop_id)
            copy_items = getattr(copy, prop_id)
            prop_srna_type = prop.srna

            if prop_srna_type:
                new_parameters = dict()
                for param in prop_srna_type.functions['new'].parameters:
                    if param.type in {'POINTER', 'COLLECTION'}: continue
                    if hasattr(param, 'enum_items'):
                        new_parameters[param.identifier] = param.enum_items[0].identifier
                        continue
                    new_parameters[param.identifier] = getattr(param, 'default_array', None) or param.default
                
                # remove as many elements as possible
                try:
                    [copy_items.remove(item) for item in copy_items[1:]]
                except:
                    pass
                
                # add enough elements to match original's count
                try:
                    for _ in range(len(original_items) - len(copy_items)):
                        copy_items.new(**new_parameters)
                        pass
                except Exception as e:
                    pass

            for og_item, copy_item in zip(original_items, copy_items):
                recursive_property_setter(op, og_item, copy_item, og_item.bl_rna.properties, False)
            
        else:
            if prop.is_readonly:
                continue
            if not ((hasattr(original, prop_id)) and (hasattr(copy, prop_id))):
                continue
            try:
                setattr(copy, prop_id, getattr(original, prop_id))
            except Exception as e:
                op.setter_fail_count += 1
                traceback.print_exc()
                print(f'GCN: Could not set path {repr(copy)}, {prop_id} of types {type(copy)}, {type(prop)} with value {getattr(original, prop_id)}! Report to developer!\n')


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
        if isinstance(
            node, 
            (
                bpy.types.NodeGroupInput,
                bpy.types.NodeGroupOutput,
                bpy.types.CompositorNodeRLayers
            )
        ):
            dst_nodes.append(None)
            continue
        new_node = dst_node_tree.nodes.new(node.bl_idname)
        if hasattr(node, 'node_tree'):
            new_node.node_tree = node.node_tree
        dst_nodes.append(new_node)
        map_og_to_copy[node] = new_node


    # extra step
    # ensure node pairs are indeed paired
    for node in filter(lambda a: hasattr(a, 'paired_output'), src_nodes):
        new_node = map_og_to_copy[node]
        new_node.pair_with_output(map_og_to_copy[node.paired_output])
        

    # 2.
    # recursively set the properties of the copied nodes, using the original nodes as a reference
    # set the parent to make positioning more convenient
    for node, new_node in zip(src_nodes, dst_nodes):
        if isinstance(
            node, 
            (
                bpy.types.NodeGroupInput,
                bpy.types.NodeGroupOutput,
                bpy.types.CompositorNodeRLayers
            )
        ):
            continue
        setattr(new_node, 'parent', map_og_to_copy.get(node.parent, None))

        if isinstance(node, bpy.types.NodeReroute):
            new_node.location = node.location
            new_node.label = node.label
            continue

        recursive_property_setter(op, node, new_node, node.bl_rna.properties, True)

    map_socket_indices = dict()
    reroute_group_placeholders: list[bpy.types.NodeReroute] = list()

    # 3.
    # update the mapping for original sockets to the copied sockets
    # it seems that sockets can get offset in memory, meaning dictionary keys and values can get changed. this appears to happen especially when forming links.
    # as a work around, let's store the socket indices and the node names, so that we have str & int variables to reference BPY data.
    # when it comes to figuring out where to make links, we can create a "task list" using the str & int variables, which are not subject to change.

    # along with remapping sockets, we need to replace group input/output nodes with reroute nodes. for obvious reasons of course
    for node, counter_part in zip(src_nodes, dst_nodes):
        if isinstance(node, (bpy.types.NodeGroupInput, bpy.types.CompositorNodeRLayers)):
            offsetY = 0
            for out in node.outputs:
                node_intersection = set(src_nodes).intersection(
                    set(map(lambda a: a.to_node, out.links))
                )
                if not node_intersection:
                    continue
                reroute = dst_node_tree.nodes.new('NodeReroute')
                reroute.name = 'group_input_placeholder'
                reroute.label = out.name
                reroute.parent = map_og_to_copy.get(node.parent, None)
                reroute.location = node.location
                reroute.location[1] += offsetY
                route_out = reroute.outputs[0]
                map_og_to_copy[out] = route_out
                map_socket_indices[route_out] = 0

                offsetY -= 30
                reroute_group_placeholders.append(reroute)
            continue

        elif isinstance(node, bpy.types.NodeGroupOutput):
            offsetY = 0 
            for inp in node.inputs:
                node_intersection = set(src_nodes).intersection(
                    set(map(lambda a: a.from_node, inp.links))
                )
                if not node_intersection:
                    continue
                reroute = dst_node_tree.nodes.new('NodeReroute')
                reroute.name = 'group_output_placeholder'
                reroute.label = inp.name
                reroute.parent = map_og_to_copy.get(node.parent, None)
                reroute.location = node.location
                reroute.location[1] += offsetY
                route_inp = reroute.inputs[0]
                map_og_to_copy[inp] = route_inp
                map_socket_indices[route_inp] = 0
                offsetY -= 30
                reroute_group_placeholders.append(reroute)
            continue


        for n, inps in enumerate(zip(node.inputs, counter_part.inputs)):
            inp1, inp2 = inps
            if inp2.hide: continue
            map_og_to_copy[inp1] = inp2
            map_socket_indices[inp2] = n

        for n, outs in enumerate(zip(node.outputs, counter_part.outputs)):
            out1, out2 = outs
            if out2.hide: continue
            map_og_to_copy[out1] = out2
            map_socket_indices[out2] = n

    # 4.
    # prepare to make links by using str & int variables to later access the necessary nodes and sockets.
    # str & int variables are not subject to change, so let's map out the links, then actually make the links

    link_tasks: list[
        tuple[
            str,
            int,
            str,
            int
        ]
    ] = list()

    for n, link in enumerate(src_node_tree.links):
        if not ((link.from_node in src_nodes) and (link.to_node in src_nodes)):
            continue
        out = map_og_to_copy[link.from_socket]
        out_ind = map_socket_indices[out]
        out_node = out.node.name

        inp = map_og_to_copy[link.to_socket]
        inp_ind = map_socket_indices[inp]
        inp_node = inp.node.name

        link_tasks.append((out_node, out_ind, inp_node, inp_ind))

    # 5.
    # make links!
    for out_node, out_ind, inp_node, inp_ind in link_tasks:
        from_socket = dst_node_tree.nodes[out_node].outputs[out_ind]
        to_socket = dst_node_tree.nodes[inp_node].inputs[inp_ind]

        dst_node_tree.links.new(from_socket, to_socket)

    while None in dst_nodes:
        dst_nodes.remove(None)
    dst_nodes.extend(reroute_group_placeholders)

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
            node_copy_buffer_path = os.path.join(preferences.custom_copy_path, BUFFER_BLEND)
            last_path_data = os.path.join(preferences.custom_copy_path, BUFFER_NAME)
        else:
            node_copy_buffer_path = os.path.join(WRITE_PATH, BUFFER_BLEND)
            last_path_data = os.path.join(WRITE_PATH, BUFFER_NAME)

        map_og_to_copy.clear()

        blend_data = context.blend_data
        node_tree: bpy.types.NodeTree = context.space_data.edit_tree

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
        node_group[UNIQUE_ID_PROP_NAME] = unique_id

        copy_nodes_to_node_tree(self, node_tree, node_group, selected_nodes)

        if self.setter_fail_count:
            self.report({'ERROR'}, f'Failed to set {self.setter_fail_count} value(s). Open console and report errors to developer!')

        map_og_to_copy.clear()

        blend_data.libraries.write(node_copy_buffer_path, {node_group}, path_remap='ABSOLUTE', compress=True)

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

    move_modal: BoolProperty(name='Move Modal', default=False, description='After pasting, use the mouse to position the nodes')
    last_pos: Vector = None
    original_offset: Vector = None
    selected_nodes: list[bpy.types.Node] = None

    loc_array: np.ndarray = None
    mask_array: np.ndarray = None
    node_count: int = 0

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
        self.original_offset = mouse_pos
        self.last_pos = mouse_pos
        return self.execute(context)
    
    def offset_node_position(self, context, offset):
        node_tree: bpy.types.NodeTree = context.space_data.edit_tree

        offset = np.full((self.node_count, 2), offset, dtype=np.float32)
        offset = np.where(
            self.mask_array[:, None],
            offset,
            0.0
        )
        self.loc_array += offset

        node_tree.nodes.foreach_set('location', self.loc_array.ravel())
        context.area.tag_redraw()
    
    def modal(self, context, event):
        context.window.cursor_modal_set('SCROLL_XY')
        new_pos = self.get_mouse_in_region(context, [event.mouse_region_x, event.mouse_region_y])

        if event.type == 'MOUSEMOVE':
            new_pos = self.get_mouse_in_region(context, [event.mouse_region_x, event.mouse_region_y])
            offset = new_pos - self.last_pos
            self.offset_node_position(context, offset)
            self.last_pos = new_pos
            return {'RUNNING_MODAL'}

        if event.type in {'MIDDLEMOUSE', 'WHEELUPMOUSE', 'WHEELDOWNMOUSE'}:
            new_pos = self.get_mouse_in_region(context, [event.mouse_region_x, event.mouse_region_y])
            offset = new_pos - self.last_pos
            self.offset_node_position(context, offset)
            self.last_pos = new_pos
            return {'PASS_THROUGH'}

        if event.type in {'RIGHTMOUSE', 'ESC'}:
            offset = self.original_offset - self.last_pos
            self.offset_node_position(context, offset)
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
        blend_data = context.blend_data
        node_tree: bpy.types.NodeTree = context.space_data.edit_tree
        if node_tree == None: return {'CANCELLED'}

        self.setter_fail_count = 0
        preferences = context.preferences.addons[BASE_PACKAGE].preferences

        if preferences.use_custom_copy_path:
            if not os.path.exists(preferences.custom_copy_path):
                self.report({'ERROR'}, 'The add-on\'s "Custom Copy Path" does not exist! Set it correctly in preferences!')
                return {'CANCELLED'}
            node_copy_buffer_path = os.path.join(preferences.custom_copy_path, BUFFER_BLEND)
            last_path_data = os.path.join(preferences.custom_copy_path, BUFFER_NAME)

        else:
            node_copy_buffer_path = os.path.join(WRITE_PATH, BUFFER_BLEND)
            last_path_data = os.path.join(WRITE_PATH, BUFFER_NAME)

        map_og_to_copy.clear()

        node_tree_to_center = node_tree
        tree_identifier = node_tree.bl_idname

        existing_node_buffer = blend_data.node_groups.get(NODE_GROUP_NAME, dict())
        current_unique_id = existing_node_buffer.get(UNIQUE_ID_PROP_NAME, '')

        if os.path.exists(last_path_data):
            with open(last_path_data, 'r') as file:
                last_copied_path, unique_id, buffer_tree_identifier = file.read().split('\n')

            if buffer_tree_identifier != tree_identifier:
                self.report({'ERROR'}, 'Copied node tree type does not match the active node tree!')
                return {'CANCELLED'}
            
            # if True, we have the most recent copy, but we should prioritize internal copy buffer. as a fallback, use the existing copy buffer, as it exists
            # update: it might be better to separate the global and internal copy buffers, letting users make the most of either. regardless, the global copy buffer can behave the same as the internal, as it won't duplicate data
            # if False, we do not have the most recent copy. delete the copy buffer node group if it exists, then load the latest
            if unique_id == current_unique_id:
                copied_node_tree = existing_node_buffer
            else:
                if existing_node_buffer:
                    blend_data.node_groups.remove(existing_node_buffer)
                if bpy.app.version >= (5, 0, 0):
                    with blend_data.libraries.load(node_copy_buffer_path, relative=True, reuse_local_id=True) as (src, dst):
                        dst.node_groups = [NODE_GROUP_NAME]
                else:
                    with blend_data.libraries.load(node_copy_buffer_path, relative=True) as (src, dst):
                        dst.node_groups = [NODE_GROUP_NAME]
                copied_node_tree = dst.node_groups[0]
                
        else:
            self.report({'ERROR'}, 'The global clipboard is empty')
            return {'CANCELLED'}

        copied_node_tree.use_fake_user = True

        new_nodes = copy_nodes_to_node_tree(self, copied_node_tree, node_tree, list(copied_node_tree.nodes))
        self.selected_nodes = new_nodes

        if self.setter_fail_count:
            self.report({'ERROR'}, f'Failed to set {self.setter_fail_count} value(s). Open console and report errors to developer!')

        [setattr(node, 'select', False) for node in node_tree.nodes]
        [setattr(node, 'select', True) for node in new_nodes]

        map_og_to_copy.clear()

        if preferences.link_to_scene:
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
            print('Global Copy Nodes: Drawing region to get node dimensions')
            bpy.ops.wm.redraw_timer(type='DRAW', iterations=1)
            context.window_manager.modal_handler_add(self)
            self.node_count = len(node_tree.nodes)
            self.loc_array = np.empty((self.node_count, 2), dtype=np.float32)
            self.mask_array = np.array([(node in new_nodes) and not node.parent for node in node_tree.nodes], dtype=bool)
            node_tree.nodes.foreach_get('location', self.loc_array.ravel())

            center = get_center_location_of_nodes(new_nodes)
            offset = self.last_pos - center

            self.offset_node_position(context, offset)
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

def register():
    r()
    bpy.types.NODE_MT_node.append(draw_operators)

    wm = bpy.context.window_manager
    kc = wm.keyconfigs.addon

    if kc:
        km = kc.keymaps.new(name='Node Editor', space_type='NODE_EDITOR', region_type='WINDOW')

        kmi = km.keymap_items.new(node_OT_global_clipboard_copy.bl_idname, 'C', 'PRESS', shift=True, ctrl=True, repeat=False)
        addon_keymaps.append((km, kmi))

        kmi = km.keymap_items.new(node_OT_global_clipboard_paste.bl_idname, 'V', 'PRESS', shift=True, ctrl=True, repeat=False)
        addon_keymaps.append((km, kmi))

def unregister():
    ur()
    bpy.types.NODE_MT_node.remove(draw_operators)

    for km, kmi in addon_keymaps:
        km.keymap_items.remove(kmi)
    addon_keymaps.clear()