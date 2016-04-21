package com.jtransc.types

import com.jtransc.ast.*
import com.jtransc.error.invalidOp

fun Baf.toAst(): AstStm {
	fun cast(type: AstType, expr: AstExpr): AstExpr {
		return AstExpr.CAST(expr, type)
	}

	return when (this) {
		is Baf.LABEL -> AstStm.STM_LABEL(this.label)
		is Baf.LINE -> AstStm.LINE(this.line)
		is Baf.THIS -> AstStm.SET(this.target.expr, cast(this.target.type, AstExpr.THIS((this.target.type as AstType.REF).name)))
		is Baf.PARAM -> AstStm.SET(this.target.expr, cast(this.target.type, AstExpr.PARAM(this.argument)))
		is Baf.IMMEDIATE -> AstStm.SET(this.target.expr, AstExpr.LITERAL(this.value))
	// @TODO: Unify?
		is Baf.COPY -> AstStm.SET(this.target.expr, cast(this.target.type, this.right.expr))
		is Baf.CAST -> AstStm.SET(this.target.expr, cast(this.target.type, this.right.expr))
		is Baf.CHECK_CAST -> AstStm.SET(this.target.expr, cast(this.target.type, this.instance.expr))
		is Baf.UNOP -> AstStm.SET(this.target.expr, AstExpr.UNOP(this.op, this.right.expr))
		is Baf.BINOP -> AstStm.SET(this.target.expr, AstExpr.BINOP(target.type, this.left.expr, this.op, this.right.expr))
		is Baf.FIELD_STATIC_GET -> AstStm.SET(this.target.expr, cast(this.target.type, AstExpr.STATIC_FIELD_ACCESS(this.field)))
		is Baf.FIELD_STATIC_SET -> AstStm.SET_FIELD_STATIC(this.field, cast(this.field.type, this.value.expr))
		is Baf.FIELD_INSTANCE_SET -> AstStm.SET_FIELD_INSTANCE(this.field, cast(this.field.containingTypeRef, this.instance.expr), cast(this.field.type, this.value.expr))
		is Baf.FIELD_INSTANCE_GET -> AstStm.SET(this.target.expr, cast(this.target.type, AstExpr.INSTANCE_FIELD_ACCESS(this.field, cast(this.field.containingTypeRef, this.instance.expr))))
		is Baf.ARRAY_LOAD -> AstStm.SET(this.target.expr, AstExpr.ARRAY_ACCESS(cast(this.target.type.array, this.array.expr), this.index.expr))
		is Baf.ARRAY_STORE -> AstStm.SET_ARRAY(cast(this.elementType.array, this.array.expr), this.index.expr, this.expr.expr)
		is Baf.ARRAY_LENGTH -> AstStm.SET(this.target.expr, AstExpr.ARRAY_LENGTH(this.array.expr))
		is Baf.IF_GOTO -> AstStm.IF_GOTO(this.label, this.cond.expr)
		is Baf.GOTO -> AstStm.IF_GOTO(this.label, null)
		is Baf.SWITCH_GOTO -> AstStm.SWITCH_GOTO(this.subject.expr, this.defaultLabel, this.labels)
		is Baf.IINCR -> AstStm.SET(this.local.expr, this.local.expr + AstExpr.LITERAL(this.incr))
		is Baf.NEW -> AstStm.SET(this.target.expr, AstExpr.NEW(this.target.type as AstType.REF))
		is Baf.NEW_ARRAY -> AstStm.SET(this.target.expr, AstExpr.NEW_ARRAY(this.arrayType, this.lengths.map { it.expr }))
		is Baf.INSTANCE_OF -> AstStm.SET(this.target.expr, AstExpr.INSTANCE_OF(this.instance.expr, this.checkType))
		is Baf.MONITOR_ENTER -> AstStm.MONITOR_ENTER(this.instance.expr)
		is Baf.MONITOR_EXIT -> AstStm.MONITOR_EXIT(this.instance.expr)
		is Baf.CAUGHT_EXCEPTION -> AstStm.SET(this.target.expr, cast(this.target.type, AstExpr.CAUGHT_EXCEPTION(this.target.type)))
		is Baf.RETURN_VOID -> AstStm.RETURN(null)
		is Baf.RETURN -> AstStm.RETURN(cast(this.rettype, this.retval.expr))
		is Baf.THROW -> AstStm.THROW(this.value.expr)
		is Baf.INVOKELIKE -> {
			val info = this.info
			val args = info.args.zip(this.info.methodRef.type.args).map { cast(it.second.type, it.first.expr) }
			val expr = if (info.instance == null) {
				AstExpr.CALL_STATIC(info.clazz, info.methodRef, args, isSpecial = info.isSpecial)
			} else {
				AstExpr.CALL_INSTANCE(cast(info.clazz, info.instance.expr), info.methodRef, args, isSpecial = info.isSpecial)
			}
			if (this is Baf.INVOKE) {
				AstStm.SET(this.target.expr, cast(this.target.type, expr))
			} else {
				AstStm.STM_EXPR(expr)
			}
		}
		else -> {
			println("Can't handle $this")
			invalidOp("Can't handle $this")
		}
	}
}

fun List<Baf>.toAst(): AstStm = AstStm.STMS(this.map { it.toAst() })

fun BafTrap.toAst(): AstTrap = AstTrap(this.start, this.end, this.handler, this.exception)

fun BafBody.toAst() : AstBody = AstBody(
	this.stms.toAst(),
	locals.getAllLocals().map { it.local },
	this.traps.map { it.toAst() }
)
