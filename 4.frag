#version 110

varying vec3 outColour;

void main()
{
        gl_FragColor = vec4(outColour, 1);
}
