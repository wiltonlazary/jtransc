class HaxePolyfills {
	{{ HAXE_METHOD_ANNOTATIONS }}
	static public function install() {
		#if js
		untyped __js__("
			Math.imul = Math.imul || function(a, b) {
			  var ah = (a >>> 16) & 0xffff;
			  var al = a & 0xffff;
			  var bh = (b >>> 16) & 0xffff;
			  var bl = b & 0xffff;
			  // the shift by 0 fixes the sign on the high part
			  // the final |0 converts the unsigned value into a signed value
			  return ((al * bl) + (((ah * bl + al * bh) << 16) >>> 0)|0);
			};
		");
		#end
	}
}