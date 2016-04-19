package com.jtransc.types

import com.jtransc.ast.*
import com.jtransc.ds.stripNulls
import com.jtransc.error.invalidOp
import com.jtransc.error.noImpl

class BafLocal(val index:Int, val type: AstType, val kind: Kind) {
	enum class Kind { TEMP, LOCAL, FRAME }

	val local = AstLocal(index, "${kind}_$index", type)
	val expr = AstExpr.LOCAL(local)
	val writeNodes = arrayListOf<Baf>()
	val readNodes = arrayListOf<Baf>()

	fun addWrite(node: Baf) {
		writeNodes.add(node)
	}

	fun addRead(node: Baf) {
		readNodes.add(node)
	}
}

class BafBody(val stms: List<Baf>, val locals: BafLocals, val traps: List<BafTrap>) {
}


class BafTrap(val start: AstLabel, val end: AstLabel, val handler: AstLabel, val exception: AstType.REF) {
}

class BafList() : Iterable<Baf> {
	override fun iterator(): Iterator<Baf> {
		return object : Iterator<Baf> {
			var next = first

			override fun hasNext(): Boolean = next != null

			override fun next(): Baf {
				val out = next!!
				next = next!!.next
				return out
			}
		}
	}

	var first: Baf? = null; private set
	var last: Baf? = null; private set


	constructor(base: Iterable<Baf>) : this() {
		for (i in base) add(i)
	}

	fun <T : Baf> add(item: T): T {
		if (last == null) {
			first = item
			last = item
		} else {
			last!!.next = item
			item.prev = last!!
			last = item
		}
		return item
	}
}

sealed class Baf {
	var prev: Baf? = null
	var next: Baf? = null
	open val writeReferences: List<BafLocal> = listOf()
	open val readReferences: List<BafLocal> = listOf()

	abstract class RESULT : Baf() {
		abstract val target: BafLocal
		override val writeReferences: List<BafLocal> by lazy { listOf(target) }
	}

	class IMMEDIATE(override val target: BafLocal, val value: Any?) : RESULT() {
	}

	class PARAM(override val target: BafLocal, val paramIndex: Int) : RESULT() {
	}

	class THIS(override val target: BafLocal) : RESULT() {
	}

	class COPY(override val target: BafLocal, val right: BafLocal) : RESULT() {
		override val readReferences = listOf(right)
	}

	class UNOP(override val target: BafLocal, val op: AstUnop, val right: BafLocal) : RESULT() {
		override val readReferences = listOf(right)
	}

	class CAST(override val target: BafLocal, val right: BafLocal) : RESULT() {
		override val readReferences = listOf(right)
	}

	class BINOP(override val target: BafLocal, val left: BafLocal, val op: AstBinop, val right: BafLocal) : RESULT() {
		override val readReferences = listOf(left, right)
	}

	class ARRAY_LOAD(override val target: BafLocal, val array: BafLocal, val index: BafLocal) : RESULT() {
		override val readReferences = listOf(array, index)
	}

	class FIELD_STATIC_GET(override val target: BafLocal, val field: AstFieldRef) : RESULT() {
	}

	class ARRAY_STORE(val array: BafLocal, val index: BafLocal, val expr: BafLocal) : Baf() {
		override val readReferences = listOf<BafLocal>(array, index, expr)
	}

	class FIELD_INSTANCE_GET(override val target: BafLocal, val ref: AstFieldRef, val instance: BafLocal) : RESULT() {
		override val readReferences = listOf<BafLocal>(instance)
	}

	class FIELD_STATIC_SET(val field: AstFieldRef, val value: BafLocal) : Baf() {
		override val readReferences = listOf<BafLocal>(value)
	}

	class FIELD_INSTANCE_SET(val ref: AstFieldRef, val instance: BafLocal, val value: BafLocal) : Baf() {
		override val readReferences = listOf<BafLocal>(instance, value)
	}

	class MONITOR_ENTER(val instance: BafLocal) : Baf() {
		override val readReferences = listOf<BafLocal>(instance)
	}

	class MONITOR_EXIT(val instance: BafLocal) : Baf() {
		override val readReferences = listOf<BafLocal>(instance)
	}

	class ARRAY_LENGTH(override val target: BafLocal, val array: BafLocal) : RESULT() {
		override val readReferences = listOf<BafLocal>(array)
	}

	class NEW(override val target: BafLocal) : RESULT() {
	}

	class NEW_ARRAY(override val target: BafLocal, val lengths: List<BafLocal>) : RESULT() {
		override val readReferences = listOf<BafLocal>() + lengths
	}

	class LINE(val line: Int) : Baf() {
	}

	class RETURN(val retval: BafLocal) : Baf() {
		override val readReferences = listOf(retval)
	}

	class RETURN_VOID() : Baf() {
	}

	class THROW(val value: BafLocal) : Baf() {
		override val readReferences = listOf(value)
	}

	/*
	class LOCAL_LOAD(override val target: BafLocal, val index: Int) : RESULT() {
	}

	class LOCAL_STORE(val index: Int, val right: BafLocal) : Baf() {
	}
	*/

	class SWITCH_GOTO(val base: BafLocal, val defaultLabel: AstLabel, labels: List<Pair<Int, AstLabel>>) : Baf() {

	}

	class GOTO(val label: AstLabel) : Baf() {
	}

	class IF_GOTO(val cond: BafLocal, val label: AstLabel) : Baf() {
		override val readReferences = listOf(cond)
	}

	class CHECK_CAST(override val target: BafLocal, val instance: BafLocal) : RESULT() {
		override val readReferences = listOf(instance)
	}

	class INSTANCE_OF(override val target: BafLocal, val instance: BafLocal) : RESULT() {
		override val readReferences = listOf(instance)
	}

	class INVOKE_VOID(val clazz: AstType.REF, val instance: BafLocal?, val methodRef: AstMethodRef, val args: List<BafLocal>, val isSpecial: Boolean) : Baf() {
		override val readReferences = (listOf(instance).stripNulls() + args)
	}

	class INVOKE(override val target: BafLocal, val clazz: AstType.REF, val instance: BafLocal?, val methodRef: AstMethodRef, val args: List<BafLocal>, val isSpecial: Boolean) : RESULT() {
		override val readReferences = (listOf(instance).stripNulls() + args)
	}

	class IINCR(val local: BafLocal, val incr: Int) : Baf() {
		override val writeReferences: List<BafLocal> = listOf(local)
		override val readReferences: List<BafLocal> = listOf(local)
	}

	class INVOKE_DYNAMIC(override val target: BafLocal, val astMethodWithoutClassRef: AstMethodWithoutClassRef, val ast: AstMethodRef, val map: List<Any?>) : RESULT() {

	}

	class LABEL(val label: AstLabel) : Baf() {
	}

	class CAUGHT_EXCEPTION(override val target: BafLocal) : RESULT() {

	}
}

//val AstLocal.expr: AstExpr.LOCAL get() = AstExpr.LOCAL(this)

//fun BafLocal.toAst(): AstExpr.LValueExpr {
//	return AstExpr.LOCAL(this.local)
//}

fun Baf.toAst(): AstStm {
	return when (this) {
		is Baf.THIS -> AstStm.SET(this.target.expr, AstExpr.THIS((this.target.type as AstType.REF).name))
		is Baf.IMMEDIATE -> AstStm.SET(this.target.expr, AstExpr.LITERAL(this.value))
		is Baf.LABEL -> AstStm.STM_LABEL(this.label)
		is Baf.LINE -> AstStm.LINE(this.line)
		is Baf.COPY -> AstStm.SET(this.target.expr, this.right.expr)
		is Baf.UNOP -> AstStm.SET(this.target.expr, AstExpr.UNOP(this.op, this.right.expr))
		is Baf.BINOP -> AstStm.SET(this.target.expr, AstExpr.BINOP(target.type, this.left.expr, this.op, this.right.expr))
		is Baf.INVOKE_VOID -> {
			if (this.instance == null) {
				//AstStm.STM_EXPR(AstExpr.CALL_STATIC())
				noImpl
			} else {
				AstStm.STM_EXPR(AstExpr.CALL_INSTANCE(instance.expr, this.methodRef, this.args.map { it.expr }, isSpecial = this.isSpecial))
			}
		}
		is Baf.RETURN_VOID -> AstStm.RETURN(null)
		is Baf.RETURN -> AstStm.RETURN(this.retval.expr)
		is Baf.FIELD_STATIC_GET -> AstStm.SET(this.target.expr, AstExpr.STATIC_FIELD_ACCESS(this.field))
		is Baf.FIELD_STATIC_SET -> AstStm.SET_FIELD_STATIC(this.field, this.value.expr)
		else -> {
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
