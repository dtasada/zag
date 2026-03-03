import re

def to_snake_case(name):
    s1 = re.sub('(.)([A-Z][a-z]+)', r'\1_\2', name)
    return re.sub('([a-z0-9])([A-Z])', r'\1_\2', s1).lower()

with open('src/raylib.zag', 'r') as f:
    content = f.read()

lines = content.split('\n')
new_lines = []
for line in lines:
    match = re.match(r'pub fn ([A-Z]\w+)\((.*)\) (.*);', line)
    if match:
        pascal_name = match.group(1)
        params_str = match.group(2)
        return_type = match.group(3).strip()

        snake_name = to_snake_case(pascal_name)
        
        params = []
        if params_str:
            param_parts = [p.strip() for p in params_str.split(',')]
            i = 0
            while i < len(param_parts):
                param = param_parts[i]
                if ':' in param:
                    name, type = param.split(':')
                    params.append(name.strip())
                else:
                    params.append(param)
                i += 1
        
        call_params = ', '.join(params)

        # Check if we need to cast the return value
        if return_type in ["Vector2", "Vector3", "Vector4", "Color", "Rectangle", "Image", "Texture", "RenderTexture", "NPatchInfo", "GlyphInfo", "Font", "Camera3D", "Camera", "Camera2D", "Mesh", "Shader", "MaterialMap", "Material", "Transform", "BoneInfo", "Model", "ModelAnimation", "Ray", "RayCollision", "BoundingBox", "Wave", "AudioStream", "Sound", "Music", "VrDeviceInfo", "VrStereoConfig", "FilePathList", "AutomationEvent", "AutomationEventList"]:
            new_line = f"pub fn {snake_name}({params_str}) {return_type} -> cast<{return_type}>(raylibc.{pascal_name}({call_params}))"
        else:
            new_line = f"pub fn {snake_name}({params_str}) {return_type} -> raylibc.{pascal_name}({call_params})"
        new_lines.append(new_line)
    else:
        new_lines.append(line)

with open('src/raylib.zag', 'w') as f:
    f.write('\n'.join(new_lines))