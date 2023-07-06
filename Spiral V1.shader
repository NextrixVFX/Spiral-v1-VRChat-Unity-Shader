Shader "!nextrix/Spiral v1"
{
    Properties 
  { 
      [HideInInspector] shader_is_using_thry_editor("", Float)=0
		[HideInInspector] shader_master_label("<color=#6CA0DC>Nextrix</color><color=#89CFF0>VFX</color>", Float) = 0
		[HideInInspector] shader_presets("SpiralPresets", Float) = 0
		[HideInInspector] shader_properties_label_file("SpiralLabel", Float) = 0
		[HideInInspector] footer_github("", Float) = 0
		[HideInInspector] footer_youtube("", Float) = 0

		[HideInInspector] m_start_SM ("Spiral Movement", Float) = 0
		[Toggle(DIRECTION_TOGGLE)] _dirTog("Flip Direction", float) = 0
		_length("Length", Range(0,2)) = 0.8
		_zoomIn("Zoom In", Range(0.001,1)) = 1
		_zoomInOut("Zoom Out", Range(0.001,1)) = 0.25
		_min("Minimum Distance", Range(0, 100)) = 0
		_max("Maximum Distance", Range(0, 100)) = 50
		[Space(2)]
		[HideInInspector] m_end_SM ("", Float) = 0
		
		
		[HideInInspector] m_start_UV ("Spiral UV", Float) = 0
		_alpha("Alpha", Range(0,1)) = 1
		_Vignett("Vignette", Range(0,1)) = 0
		_vigSmooth("Vignette Smooth", Range(1.25,5)) = 3.5
		_blur("Detail Blur", Range(0,1)) = 1
		_comp("Complexity", Range(-5,5)) = 1
		_scale("Scale", Range(-1,20)) = 5
		_speed("Speed", Range(-10,10)) = 1
		[Space(2)]
		[HideInInspector] m_end_UV ("", Float) = 0
		
		
		[HideInInspector] m_start_CL ("Spiral Coloring", Float) = 0
		[Toggle(CENTER_TOGGLE)] _centerTog("Toggle Center", float) = 1
		[HDR]_centerColor("Center Color", Color) = (0.6,0.6,0.6)
		[HDR]_linesColor("Line Color", Color) = (0,0,0)
		[HDR]_HlinesColor("Line Highlights Color", Color) = (1,1,1)
		_invert("Invert", Range(-1,1)) = -0.4
		_power("Power", Range(-1,10)) = 2
		[Space(5)]
		[HideInInspector] m_start_RB ("Rainbow", Float) = 0
		[Toggle(RAINBOW_TOGGLE)] _rTog("Toggle Rainbow", float) = 0
		_rb("Rainbow", range(0, 1)) = 1
        _rbo("Rainbow Offset", float) = 0
        _rbs("Rainbow Speed", range(0, 1)) = 0
		[Space(2)]
		[HideInInspector] m_end_RB ("", Float) = 0
		[HideInInspector] m_end_CL ("", Float) = 0

		[HideInInspector] m_start_renderingSettings ("Rendering Settings", Float) = 0
		[Toggle] _PARTICLE("Use For Particle System", int) = 0
        [Enum(UnityEngine.Rendering.CullMode)] _Cull ("Cull", Float) = 0
        [Enum(UnityEngine.Rendering.CompareFunction)] _ZTest ("ZTest", Float) = 8
        [Enum(UnityEngine.Rendering.BlendMode)] _SourceBlend ("Source Blend", Float) = 1
        [Enum(UnityEngine.Rendering.BlendMode)] _DestinationBlend ("Destination Blend", Float) = 1
        [Enum(Off, 0, On, 1)] _ZWrite ("ZWrite", Int) = 0
		[HideInInspector]TileFactor("Tile XY / Offset ZW", vector) = (1,1,0,0)
		[Space(2)]
		[HideInInspector] m_end_renderingSettings ("", Float) = 0
		
		
		
	}
	CustomEditor "Thry.ShaderEditor"
		SubShader
		{
			//Tags {"Queue" = "Overlay+69420"}
			//GrabPass {"_Grab"} Cull Off ZWrite Off ZTest Always blend one one
			LOD 100
			ZWrite [_ZWrite]
			Cull [_Cull]
			ZTest [_ZTest]
			Blend [_SourceBlend] [_DestinationBlend]
			GrabPass{"_Grab"}
			Tags{ "LightMode" = "Always""RenderType" = "Overlay"  "Queue" = "Overlay+2147479647""IsEmissive" = "true" "DisableBatching"= "true" "ForceNoShadowCasting" = "true" "IgnoreProjector" = "true" "PreviewType"="Plane"} 
			Pass
			{
				CGPROGRAM
				#define NEAR _min
				#define FAR _max
				#define MAX_STEPS 64 * _blur

				#define PI 3.14159265359
				#define EPS 0.001

				#define mix lerp
				#define iTime _Time.y
				#define fract frac
				#define mod(x, y) (x-y* floor(x/y))
				#pragma vertex vert
				#pragma fragment frag
				#pragma shader_feature CENTER_TOGGLE
				#pragma shader_feature DIRECTION_TOGGLE
				#pragma shader_feature RAINBOW_TOGGLE
				#include "UnityCG.cginc"

				struct appdata
				{
					float4 vertex		: POSITION;
					float4 vertexColor	: COLOR;
					float3 cen			: TEXCOORD0;
				};

			struct v2f
			{
				float4 vertex		: SV_POSITION;
				float3 uv			: TEXCOORD0;
				float dstf : TEXCOORD1;
				float vertexColor : COLOR;
			};

				float _zoomIn, _alpha, _Vignett, _vigSmooth, _zoomInOut, _blur, _length, _power, _invert, _comp;
				float _min, _max, _scale, _rb, _rbo, _rbs, _rTog;
				uniform float _fe, _fs, _speed;
				uniform int _PARTICLE;
				uniform float _centerTog, _dirTog;
				uniform float4 TileFactor, _centerColor, _linesColor, _HlinesColor;

				inline bool IsInMirror()
				{
					return unity_CameraProjection[2][0] != 0.f || unity_CameraProjection[2][1] != 0.f;
				}

				inline float3 cPos()
				{
	#if UNITY_SINGLE_PASS_STEREO
					return (unity_StereoWorldSpaceCameraPos[0] + unity_StereoWorldSpaceCameraPos[1]) * 0.5;
	#else
					return _WorldSpaceCameraPos;
	#endif
				}

				v2f vert(appdata v)
				{
					v2f o;
					float4 pos = 0.0, cen = 0.0;
					float dist = 0.0;
					UNITY_BRANCH if (_PARTICLE)
					{
						pos = mul(unity_WorldToObject, mul(unity_CameraToWorld, v.vertex - float4(v.cen, 0)));
						dist = distance(cPos(), v.cen);
						cen = float4(v.cen, 1.0);
						o.vertexColor = v.vertexColor.w;
					}
					else
					{
						pos = mul(unity_WorldToObject, mul(unity_CameraToWorld, v.vertex));
						dist = distance(cPos(), mul(unity_ObjectToWorld, float4(0.0, 0.0, 0.0, 1.0)));
						cen = float4(0.0, 0.0, 0.0, 1.0);
						o.vertexColor = 1.0;
					}
					o.vertex = UnityObjectToClipPos(pos);
					o.uv = mul(UNITY_MATRIX_V, mul(unity_ObjectToWorld, pos).xyz - cPos());
					if (IsInMirror())
						o.vertex = 0.0;
					return o;
				}


			float3 HSVToRGB(float3 c)
			{
				float4 K = float4(1.0, 2.0 / 3.0, 1.0 / 3.0, 3.0);
				float3 p = abs(frac(c.xxx + K.xyz) * 6.0 - K.www);
				return c.z * lerp(K.xxx, saturate(p - K.xxx), c.y);
			}

			// Hash by iq
			float hash(float2 p) {
				float h = 1.0 + dot(p, float2(127.1, 311.7));
				return fract(cos(h) * 43758.5453123);
			}

			float rbox(float3 p, float3 s, float r) {
				return length(max(abs(p) - s + r, 0.0)) - r * _scale;
			}


			float2 rot(float2 k, float t) {
				#ifdef DIRECTION_TOGGLE
					float ct = sin(t / _comp);
					float st = cos(t / _comp);
				#else
					float ct = cos(t / _comp);
					float st = sin(t / _comp);
				#endif
				return float2(ct * k.x - st * k.y, st * k.x + ct * k.y);
			}

			void oprep2(inout float2 p, float q, float s, float k) {
				float r = 1.0 / q;
				float ofs = s;
				float angle = atan2(p.x, p.y);
				float a = mod(angle, 2.0 * PI * r) - PI * r;
				p.xy = float2(sin(a), cos(a)) * length(p.xy) - ofs;
				p.x += ofs;
			}

			float map(float3 p) {
				p.y -= 1.0;
				p.xy = rot(p.xy, p.z * 0.15);
				p.z += iTime * _speed;
				p.xy = mod(p.xy, 6.0) - 0.5 * 6.0;
				p.xy = rot(p.xy, -floor(p.z / 0.75) * 0.35);
				p.z = mod(p.z, 0.75) - 0.5 * 0.75;
				oprep2(p.xy, 6.0, 0.45, iTime);

				return rbox(p, float3(0.1, 0.025, 0.25), 0.05);
			}

			float3 getNormal(float3 p) {
				float h = 0.0001;

				return normalize(
					float3(map(p + float3(h, 0, 0)) - map(p - float3(h, 0, 0)),
						map(p + float3(0, h, 0)) - map(p - float3(0, h, 0)),
						map(p + float3(0, 0, h)) - map(p - float3(0, 0, h))));
			}

			float saw(float x, float d, float s, float shift) {
				float xp = PI * (x * d + iTime * 0.5 + shift);

				float as = asin(s);
				float train = 0.5 * sign(sin(xp - as) - s) + _invert;

				float range = (PI - 2.0 * as);
				xp = mod(xp, 2.0 * PI);
				float y = mod(-(xp - 2.0 * as), range) / range;
				y *= train;

				return y;
			}

			float3 getShading(float3 p, float3 normal, float3 lightPos) {
				//rainbow
				float is = 1.0; /// _samples;
				float3 rc = HSVToRGB(float3(_rbo + _Time.y * _rbs, _rb, 0.5));
				
				float3 lightDirection = normalize(lightPos - p);
				float lightIntensity = clamp(dot(normal, lightDirection), 0.0, 1.0);

				float2 id = floor((p.xy + 3.0) / 6.0);
				float fid = hash(id);
				float ve = hash(id);
				//float3 col = _HlinesColor;
				#ifdef RAINBOW_TOGGLE
					float3 col = rc;
					//float3 amb = rc;
				#else
					float3 col = _HlinesColor;
					
				#endif
				float3 amb = _linesColor;
				col *= 4.0 * saw(p.z, 0.092, 0.77, fid * 1.5);
				float3 tex = float3(0.8098039, 0.8607843, 1.0);

				return col * tex * lightIntensity + amb * (1.0 - lightIntensity);
			}

			void raymarch(float3 ro, float3 rd, out int i, out float t) {
				t = 0.0;

				for (int j = 0; j < MAX_STEPS; ++j) {
					float3 p = ro + rd * t;
					float h = map(p);
					i = j;

					t += h * 0.7;
				}
			}

			float computecenter(float3 ro, float3 rd, float t, float lp) {
				float3 lpos = float3(0.0, 0.0, 54.0);
				ro -= lpos;
				float m = dot(rd, -ro);
				float d = length(ro - float3(0.0, 0.0, 0.7) + m * rd);

				float a = -m;
				float b = t - m;
				float aa = atan(a / d);
				float ba = atan(b / d);
				float to = (ba - aa) / d;

				return to * 0.15 * lp;
			}

			float3 computeColor(float3 ro, float3 rd) {
				int i;
				float t;
				raymarch(ro, rd, i, t);
				float3 rc = HSVToRGB(float3(_rbo + _Time.y * _rbs, _rb, 1));
				float lp = sin(iTime - 1.0) + _power;
				
				#ifdef CENTER_TOGGLE
					float3 color = _centerColor;
				#else
					float3 color = float3(0.0, 0.0, 0.0);
				#endif

				if (i < MAX_STEPS && t >= NEAR && t <= FAR) {
					float3 p = ro + rd * t;
					float3 normal = getNormal(p);

					float z = 1.0 - (NEAR + t) / (FAR - NEAR);

					color = getShading(p, normal, 0.0);
					color *= lp;

					float zSqrd = z * z;
					
					color = mix(0.0, color, zSqrd * (3.0 - 2.0 * z));


					color += computecenter(ro, rd, t, lp);
					return pow(color, 0.8);
				}
				return color * computecenter(ro, rd, t, lp);
			}

			fixed4 frag(v2f i) : SV_Target
			{
				
				float2 q = (i.uv.xy / abs(i.uv.z));
				q *= _zoomIn;
				float dstf = saturate(i.dstf * i.vertexColor) * smoothstep(0 + _Vignett * 1.2 * TileFactor.xy, _vigSmooth * _Vignett * TileFactor.xy, length(q));
				float2 coord = 2.0 * q - 0.0;
				coord *= 0.84;

				float3 dir = float3(0.0, 0.0, 1.0) * _zoomInOut;
				float3 up = float3(0.0, 1.0, 0.0);

				float3 right = normalize(cross(dir, up));

				float3 ro = float3(0.0, 1.0, 8.74);
				float3 rd = normalize(dir * 2.0 + coord.x * right + coord.y * up) * _length;
				float3 col = computeColor(ro, rd);

				return float4(col, 1.0) *(0 + _alpha * 5) * dstf;
			}

            
            ENDCG
        }
    }
}
