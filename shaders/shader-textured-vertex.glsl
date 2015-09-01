// Jonatan H Sundqvist
// #version 330

#version 420

precision mediump float;

attribute vec3 aVertexPosition;
attribute vec2 aTexCoord;

uniform mat4 uMVMatrix;
uniform mat4 uPMatrix;
uniform sampler2D uSampler;

// varying vec4 vColor;
// varying highp vec2 vTexCoord;
out highp vec2 vTexCoord;


void main(void) {
	gl_Position = uPMatrix * uMVMatrix * vec4(aVertexPosition, 1.0);
	vTexCoord   = vec2(aTexCoord.s, 1.0-aTexCoord.t);
	// vColor = vec4(1.0, 0.02, 0.02, 1.0);
}