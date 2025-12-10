# Global Copy Nodes
Global Copy Nodes makes it extremely easy to copy nodes across different .blend files. It does not rely on the internal copy buffer, and can copy across many instances of Blender.

## Add-on Preferences
### Custom Copy Path
- Use Custom Copy Path (Boolean, default `False`)
  - Enables the use of a custom copy path, allowing you to copy across different.blend versions
- Custom Copy Path (String, directory type)
  - The path to use to store the global copy buffer

### Link to Scene
- Automatically Link To Scene (Boolean, default `True`)
  - Enables the automatic linking of attached objects or collections to the active scene. If disabled, they will be hidden until manually linked.
- Create Collections For Attached Objects (Boolean, default `True`)
  - If enabled, a default collection will automatically be created to nest objects or other collections under. If disabled, the scene collection will be used.

### Internal Buffer
- Copy to Internal Buffer (Boolean, default `True`)
  - If enabled, it copies nodes to the internal and global buffer.

## Add-on Operators
### Global Copy Nodes
- Copies from the global copy buffer. `Ctrl + Shift + C` by default
### Global Paste Nodes
- Pastes from the global copy buffer. `Ctrl + Shift + V` by default

# Credits
Created by hisanimations