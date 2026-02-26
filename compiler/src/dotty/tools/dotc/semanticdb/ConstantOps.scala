package dotty.tools
package dotc
package semanticdb

import dotty.tools.dotc.semanticdb.{javalite as jl}

import core.Contexts.Context
import core.Constants.*

object ConstantOps:
  extension (const: Constant)
    def toSemanticConst(using Context): jl.Constant = const.tag match {
      case UnitTag =>
        jl.Constant
          .newBuilder()
          .setUnitConstant(jl.UnitConstant.getDefaultInstance)
          .build()
      case BooleanTag =>
        jl.Constant
          .newBuilder()
          .setBooleanConstant(jl.BooleanConstant.newBuilder().setValue(const.booleanValue).build())
          .build()
      case ByteTag =>
        jl.Constant
          .newBuilder()
          .setByteConstant(jl.ByteConstant.newBuilder().setValue(const.byteValue).build())
          .build()
      case ShortTag =>
        jl.Constant
          .newBuilder()
          .setShortConstant(jl.ShortConstant.newBuilder().setValue(const.shortValue).build())
          .build()
      case CharTag =>
        jl.Constant
          .newBuilder()
          .setCharConstant(jl.CharConstant.newBuilder().setValue(const.charValue).build())
          .build()
      case IntTag =>
        jl.Constant
          .newBuilder()
          .setIntConstant(jl.IntConstant.newBuilder().setValue(const.intValue).build())
          .build()
      case LongTag =>
        jl.Constant
          .newBuilder()
          .setLongConstant(jl.LongConstant.newBuilder().setValue(const.longValue).build())
          .build()
      case FloatTag =>
        jl.Constant
          .newBuilder()
          .setFloatConstant(jl.FloatConstant.newBuilder().setValue(const.floatValue).build())
          .build()
      case DoubleTag =>
        jl.Constant
          .newBuilder()
          .setDoubleConstant(jl.DoubleConstant.newBuilder().setValue(const.doubleValue).build())
          .build()
      case StringTag =>
        jl.Constant
          .newBuilder()
          .setStringConstant(jl.StringConstant.newBuilder().setValue(const.stringValue).build())
          .build()
      case NullTag =>
        jl.Constant
          .newBuilder()
          .setNullConstant(jl.NullConstant.getDefaultInstance)
          .build()
      case _ => throw new Error(s"Constant ${const} can't be converted to Semanticdb Constant.")
    }
