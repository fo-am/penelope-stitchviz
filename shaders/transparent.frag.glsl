// a daft little fragment shader to test fluxus
// about as simple as it gets

varying vec3 fragcol;

uniform vec3 DiffuseColour;
uniform float Opacity;
varying vec3 N;
varying vec3 L;

void main()
{
    vec3 l = normalize(L);
    vec3 n = normalize(N);
    float diffuse = dot(l,n);   
    gl_FragColor = vec4(DiffuseColour*diffuse,Opacity);
}
