package com.jtransc.types

import com.jtransc.ast.*
import com.jtransc.error.invalidOp
import com.jtransc.lang.exec

fun List<Baf>.toAst(): AstStm {
	fun cast(type: AstType, expr: AstExpr): AstExpr {
		return AstExpr.CAST(expr, type)
	}
	
	val list = this
	val out = arrayListOf<AstStm>()
	fun add(stm:AstStm) = exec { out.add(stm) }
	for (item in list) {
		when (item) {
			is Baf.LABEL -> add(AstStm.STM_LABEL(item.label.ast))
			is Baf.LINE -> add(AstStm.LINE(item.line))
			is Baf.THIS -> add(AstStm.SET(item.target.expr, cast(item.target.type, AstExpr.THIS((item.target.type as AstType.REF).name))))
			is Baf.PARAM -> add(AstStm.SET(item.target.expr, cast(item.target.type, AstExpr.PARAM(item.argument))))
			is Baf.IMMEDIATE -> add(AstStm.SET(item.target.expr, AstExpr.LITERAL(item.value)))
		// @TODO: Unify?
			is Baf.COPY -> add(AstStm.SET(item.target.expr, cast(item.target.type, item.right.expr)))
			is Baf.CAST -> add(AstStm.SET(item.target.expr, cast(item.target.type, item.right.expr)))
			is Baf.CHECK_CAST -> add(AstStm.SET(item.target.expr, cast(item.target.type, item.instance.expr)))
			is Baf.UNOP -> add(AstStm.SET(item.target.expr, cast(item.target.type, AstExpr.UNOP(item.op, item.right.expr))))
			is Baf.BINOP -> {
				add(AstStm.SET(item.target.expr, cast(item.target.type, AstExpr.BINOP(
					item.target.type,
					cast(item.ltt, item.left.expr),
					item.op,
					cast(item.rtt, item.right.expr)
				))))
			}
			is Baf.FIELD_STATIC_GET -> add(AstStm.SET(item.target.expr, cast(item.target.type, AstExpr.STATIC_FIELD_ACCESS(item.field))))
			is Baf.FIELD_STATIC_SET -> add(AstStm.SET_FIELD_STATIC(item.field, cast(item.field.type, item.value.expr)))
			is Baf.FIELD_INSTANCE_SET -> add(AstStm.SET_FIELD_INSTANCE(item.field, cast(item.field.containingTypeRef, item.instance.expr), cast(item.field.type, item.value.expr)))
			is Baf.FIELD_INSTANCE_GET -> add(AstStm.SET(item.target.expr, cast(item.target.type, AstExpr.INSTANCE_FIELD_ACCESS(item.field, cast(item.field.containingTypeRef, item.instance.expr)))))
			is Baf.ARRAY_LOAD -> add(AstStm.SET(item.target.expr, AstExpr.ARRAY_ACCESS(cast(item.target.type.array, item.array.expr), item.index.expr)))
			is Baf.ARRAY_STORE -> add(AstStm.SET_ARRAY(cast(item.elementType.array, item.array.expr), item.index.expr, item.expr.expr))
			is Baf.ARRAY_LENGTH -> add(AstStm.SET(item.target.expr, AstExpr.ARRAY_LENGTH(item.array.expr)))
			is Baf.IF_GOTO -> {
				add(AstStm.IF_GOTO(item.trueLabel.ast, item.cond.expr))
				add(AstStm.IF_GOTO(item.falseLabel.ast, null))
			}
			is Baf.GOTO -> add(AstStm.IF_GOTO(item.label.ast, null))
			is Baf.SWITCH_GOTO -> add(AstStm.SWITCH_GOTO(item.subject.expr, item.defaultLabel.ast, item.labels.map { it.first to it.second.ast }))
			is Baf.IINCR -> add(AstStm.SET(item.local.expr, item.local.expr + AstExpr.LITERAL(item.incr)))
			is Baf.NEW -> add(AstStm.SET(item.target.expr, AstExpr.NEW(item.target.type as AstType.REF)))
			is Baf.NEW_ARRAY -> add(AstStm.SET(item.target.expr, AstExpr.NEW_ARRAY(item.arrayType, item.lengths.map { it.expr })))
			is Baf.INSTANCE_OF -> add(AstStm.SET(item.target.expr, AstExpr.INSTANCE_OF(item.instance.expr, item.checkType)))
			is Baf.MONITOR_ENTER -> add(AstStm.MONITOR_ENTER(item.instance.expr))
			is Baf.MONITOR_EXIT -> add(AstStm.MONITOR_EXIT(item.instance.expr))
			is Baf.CAUGHT_EXCEPTION -> add(AstStm.SET(item.target.expr, cast(item.target.type, AstExpr.CAUGHT_EXCEPTION(item.target.type))))
			is Baf.RETURN_VOID -> add(AstStm.RETURN(null))
			is Baf.RETURN -> add(AstStm.RETURN(cast(item.rettype, item.retval.expr)))
			is Baf.THROW -> add(AstStm.THROW(item.value.expr))
			is Baf.INVOKELIKE -> {
				val info = item.info
				val args = info.args.zip(item.info.methodRef.type.args).map { cast(it.second.type, it.first.expr) }
				val expr = if (info.instance == null) {
					AstExpr.CALL_STATIC(info.clazz, info.methodRef, args, isSpecial = info.isSpecial)
				} else {
					AstExpr.CALL_INSTANCE(cast(info.clazz, info.instance.expr), info.methodRef, args, isSpecial = info.isSpecial)
				}
				if (item is Baf.INVOKE) {
					add(AstStm.SET(item.target.expr, cast(item.target.type, expr)))
				} else {
					add(AstStm.STM_EXPR(expr))
				}
			}
			else -> {
				println("Can't handle $this")
				invalidOp("Can't handle $this")
			}
		}
	}
	return AstStm.STMS(out)
}

fun BafTrap.toAst(): AstTrap = AstTrap(this.start.ast, this.end.ast, this.handler.ast, this.exception)

fun BafBody.toAst() : AstBody = AstBody(
	this.stms.toAst(),
	locals.getAllLocals().map { it.local },
	this.traps.map { it.toAst() }
)
