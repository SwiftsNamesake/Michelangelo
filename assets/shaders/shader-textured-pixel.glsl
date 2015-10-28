// Jonatan H Sundqvist

#version 420

precision mediump float;

uniform sampler2D uSampler;

// varying vec4 vColor;
in highp vec2 vTexCoord;
out vec4 gl_FragColor;

void main(void) {
	// vec4 texColour = vec4(vTexCoord.s, vTexCoord.t, 0.2, 1.0);
	vec4 texColour = texture(uSampler, vec2(vTexCoord.s, vTexCoord.t));;
	gl_FragColor   = vec4(texColour.rgb, 1.0);
}
