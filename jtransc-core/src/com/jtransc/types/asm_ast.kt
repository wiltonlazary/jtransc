package com.jtransc.types

import com.jtransc.ast.AstBody
import com.jtransc.ast.AstType
import org.objectweb.asm.tree.MethodNode

fun Asm2Ast(clazz: AstType.REF, method: MethodNode): AstBody {
	return Asm2Baf(clazz, method).toAst()
}
