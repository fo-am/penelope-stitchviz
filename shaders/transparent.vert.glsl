// a daft little vertex shader to test fluxus

uniform vec3 LightPos;
varying vec3 N;
varying vec3 L;

void main()
{     
    N = normalize(gl_NormalMatrix*gl_Normal);
    L = vec3(gl_ModelViewMatrix*(vec4(LightPos,1)-gl_Vertex));
    gl_Position = ftransform();
}
