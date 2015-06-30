#version 110

attribute vec2 position;
attribute vec3 colour;
varying vec3 outColour;

void main()
{
    outColour = colour;
    gl_Position = vec4(position, 0.0, 1.0);
}
