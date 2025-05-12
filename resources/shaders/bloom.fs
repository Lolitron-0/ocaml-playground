#version 330

// Input vertex attributes (from vertex shader)
in vec2 fragTexCoord;
in vec4 fragColor;

// Input uniform values
uniform sampler2D texture0;
uniform vec4 colDiffuse;

// Output fragment color
out vec4 finalColor;

uniform float renderWidth;
uniform float renderHeight;

const float samples = 5.0;          // Pixels per axis; higher = bigger glow, worse performance
const float quality = 2.5;          // Defines size factor: Lower = smaller glow, better quality

float gamma = 0.6;
float numColors = 8.0;

float stitchingSize = 6.0;
int invert = 0;

vec4 stitch(sampler2D tex, vec2 uv)
{
    vec4 c = vec4(0.0);
    float size = stitchingSize;
    vec2 cPos = uv * vec2(renderWidth, renderHeight);
    vec2 tlPos = floor(cPos / vec2(size, size));
    tlPos *= size;

    int remX = int(mod(cPos.x, size));
    int remY = int(mod(cPos.y, size));

    if (remX == 0 && remY == 0) tlPos = cPos;

    vec2 blPos = tlPos;
    blPos.y += (size - 1.0);

    if ((remX == remY) || (((int(cPos.x) - int(blPos.x)) == (int(blPos.y) - int(cPos.y)))))
    {
        if (invert == 1) c = vec4(0.2, 0.15, 0.05, 1.0);
        else c = texture(tex, tlPos * vec2(1.0/renderWidth, 1.0/renderHeight)) * 1.4;
    }
    else
    {
        if (invert == 1) c = texture(tex, tlPos * vec2(1.0/renderWidth, 1.0/renderHeight)) * 1.4;
        else c = vec4(0.0, 0.0, 0.0, 1.0);
    }

    return c;
}

vec4 bloom(sampler2D tex, vec2 uv) {
    vec2 size = vec2(renderWidth, renderHeight);
    vec4 sum = vec4(0);
    vec2 sizeFactor = vec2(1)/size*quality;

    // Texel color fetching from texture sampler
    vec4 source = texture(tex, uv);

    const int range = 2;            // should be = (samples - 1)/2;

    for (int x = -range; x <= range; x++)
    {
        for (int y = -range; y <= range; y++)
        {
            sum += texture(tex, uv + vec2(x, y)*sizeFactor);
        }
    }

    // Calculate final fragment color
    vec4 c  = ((sum/(samples*samples)) + source)*colDiffuse*vec4(0.6);
    return c;
}

vec4 posterize(vec4 color) {
    vec4 c = color; 
    c = c*numColors;
    c = floor(c);
    c = c/numColors;
    return c;
}

void main()
{
    vec4 baseColor = bloom(texture0, fragTexCoord);

    baseColor = posterize(baseColor);

    vec4 stitchedColor = stitch(texture0, fragTexCoord);
    finalColor = mix(baseColor, stitchedColor, 0.08);
}
