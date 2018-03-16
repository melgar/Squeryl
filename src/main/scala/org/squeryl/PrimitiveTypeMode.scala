/*******************************************************************************
 * Copyright 2010 Maxime Lévesque
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *   http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 ***************************************************************************** */
package org.squeryl


import dsl.ast._
import dsl._
import internals.FieldReferenceLinker
import java.util.{ Date, UUID }
import java.sql.Timestamp
import java.sql.ResultSet
import org.squeryl.internals.Utils
import org.squeryl.internals.FieldMapper

import scalaz.{@@, NotNothing, Tag}

@deprecated("the PrimitiveTypeMode companion object is deprecated, you should define a mix in the trait for your application. See : http://squeryl.org/0.9.6.html",
    "0.9.6")
object PrimitiveTypeMode extends PrimitiveTypeMode

private [squeryl] object InternalFieldMapper extends PrimitiveTypeMode

trait PrimitiveTypeMode extends QueryDsl with FieldMapper {
  private def subst2[A, B, F[_, _], T](fab: F[A, B]): F[A @@ T, B @@ T] =
    fab.asInstanceOf[F[A @@ T, B @@ T]]

  def taggedTEF[A, Q: NotNothing, T](implicit factory: TypedExpressionFactory[A, T]): TypedExpressionFactory[A @@ Q, T @@ Q] =
    subst2[A, T, TypedExpressionFactory, Q](factory)

  def taggedToTE[A, Q, T](
    a: A @@ Q
  )(
    implicit factory: TypedExpressionFactory[A @@ Q, T @@ Q]
  ): TypedExpression[A @@ Q, T @@ Q] =
    factory.create(a)

  def taggedOptionTEF[A, Q: NotNothing, T](
    implicit factory: TypedExpressionFactory[Option[A], T]
  ): TypedExpressionFactory[Option[A @@ Q], T @@ Q] =
    subst2[A, T, ({ type L[a, b] = TypedExpressionFactory[Option[a], b] })#L, Q](factory)

  def taggedOptionToTE[A, Q, T](
    a: Option[A @@ Q]
  )(
    implicit factory: TypedExpressionFactory[Option[A @@ Q], T @@ Q]
  ): TypedExpression[Option[A @@ Q], T @@ Q] =
    factory.create(a)

  // =========================== Non Numerical ===========================
  implicit val stringTEF = PrimitiveTypeSupport.stringTEF
  implicit val optionStringTEF = PrimitiveTypeSupport.optionStringTEF
  implicit val dateTEF = PrimitiveTypeSupport.dateTEF
  implicit val optionDateTEF = PrimitiveTypeSupport.optionDateTEF
  implicit val sqlDateTEF = PrimitiveTypeSupport.sqlDateTEF
  implicit val optionSqlDateTEF = PrimitiveTypeSupport.optionSqlDateTEF
  implicit val timestampTEF = PrimitiveTypeSupport.timestampTEF
  implicit val optionTimestampTEF = PrimitiveTypeSupport.optionTimestampTEF
  implicit val doubleArrayTEF = PrimitiveTypeSupport.doubleArrayTEF
  implicit val intArrayTEF = PrimitiveTypeSupport.intArrayTEF
  implicit val longArrayTEF = PrimitiveTypeSupport.longArrayTEF
  implicit val stringArrayTEF = PrimitiveTypeSupport.stringArrayTEF
  
  // =========================== Numerical Integral =========================== 
  implicit val byteTEF = PrimitiveTypeSupport.byteTEF
  implicit val optionByteTEF = PrimitiveTypeSupport.optionByteTEF
  implicit val intTEF = PrimitiveTypeSupport.intTEF
  implicit val optionIntTEF = PrimitiveTypeSupport.optionIntTEF
  implicit val longTEF = PrimitiveTypeSupport.longTEF
  implicit val optionLongTEF = PrimitiveTypeSupport.optionLongTEF
  
  // =========================== Numerical Floating Point ===========================   
  implicit val floatTEF = PrimitiveTypeSupport.floatTEF
  implicit val optionFloatTEF = PrimitiveTypeSupport.optionFloatTEF
  implicit val doubleTEF = PrimitiveTypeSupport.doubleTEF
  implicit val optionDoubleTEF = PrimitiveTypeSupport.optionDoubleTEF  
  implicit val bigDecimalTEF = PrimitiveTypeSupport.bigDecimalTEF
  implicit val optionBigDecimalTEF = PrimitiveTypeSupport.optionBigDecimalTEF
  
  
  implicit def stringToTE(s: String) = stringTEF.create(s)  
  implicit def optionStringToTE(s: Option[String]) = optionStringTEF.create(s)

  implicit def dateToTE(s: Date) = dateTEF.create(s)
  implicit def optionDateToTE(s: Option[Date]) = optionDateTEF.create(s)

  implicit def timestampToTE(s: Timestamp) = timestampTEF.create(s)
  implicit def optionTimestampToTE(s: Option[Timestamp]) = optionTimestampTEF.create(s)

  implicit def booleanToTE(s: Boolean) = PrimitiveTypeSupport.booleanTEF.create(s)
  implicit def optionBooleanToTE(s: Option[Boolean]) = PrimitiveTypeSupport.optionBooleanTEF.create(s)

  implicit def uuidToTE(s: UUID) = PrimitiveTypeSupport.uuidTEF.create(s)
  implicit def optionUUIDToTE(s: Option[UUID]) = PrimitiveTypeSupport.optionUUIDTEF.create(s)

  implicit def binaryToTE(s: Array[Byte]) = PrimitiveTypeSupport.binaryTEF.create(s)
  implicit def optionByteArrayToTE(s: Option[Array[Byte]]) = PrimitiveTypeSupport.optionByteArrayTEF.create(s)

  implicit def enumValueToTE[A <: Enumeration#Value](e: A) =
    PrimitiveTypeSupport.enumValueTEF(e).create(e)
    
  implicit def optionEnumcValueToTE[A <: Enumeration#Value](e: Option[A]) = 
    PrimitiveTypeSupport.optionEnumValueTEF(e).create(e)
  
  implicit def byteToTE(f: Byte) = byteTEF.create(f)    
  implicit def optionByteToTE(f: Option[Byte]) = optionByteTEF.create(f)

  implicit def intToTE(f: Int) = intTEF.create(f)
  implicit def optionIntToTE(f: Option[Int]) = optionIntTEF.create(f)

  implicit def longToTE(f: Long) = longTEF.create(f)
  implicit def optionLongToTE(f: Option[Long]) = optionLongTEF.create(f)

  implicit def floatToTE(f: Float) = floatTEF.create(f)
  implicit def optionFloatToTE(f: Option[Float]) = optionFloatTEF.create(f)

  implicit def doubleToTE(f: Double) = doubleTEF.create(f)
  implicit def optionDoubleToTE(f: Option[Double]) = optionDoubleTEF.create(f)

  implicit def bigDecimalToTE(f: BigDecimal) = bigDecimalTEF.create(f)
  implicit def optionBigDecimalToTE(f: Option[BigDecimal]) = optionBigDecimalTEF.create(f)
  
  implicit def doubleArrayToTE(f : Array[Double]) = doubleArrayTEF.create(f)
  implicit def intArrayToTE(f : Array[Int]) = intArrayTEF.create(f)
  implicit def longArrayToTE(f : Array[Long]) = longArrayTEF.create(f)
  implicit def stringArrayToTE(f: Array[String]) = stringArrayTEF.create(f)
  

  implicit def logicalBooleanToTE(l: LogicalBoolean) =
    PrimitiveTypeSupport.booleanTEF.convert(l)

  // =========================== Tagged Non Numerical ===========================
  implicit def stringTaggedTEF[Q: NotNothing]: TypedExpressionFactory[String @@ Q, TString @@ Q] = taggedTEF
  implicit def optionStringTaggedTEF[Q: NotNothing]: TypedExpressionFactory[Option[String @@ Q], TOptionString @@ Q] = taggedOptionTEF
  implicit def dateTaggedTEF[Q: NotNothing]: TypedExpressionFactory[java.util.Date @@ Q, TDate @@ Q] = taggedTEF
  implicit def optionDateTaggedTEF[Q: NotNothing]: TypedExpressionFactory[Option[java.util.Date @@ Q], TOptionDate @@ Q] = taggedOptionTEF
  implicit def sqlDateTaggedTEF[Q: NotNothing]: TypedExpressionFactory[java.sql.Date @@ Q, TDate @@ Q] = taggedTEF
  implicit def optionSqlDateTaggedTEF[Q: NotNothing]: TypedExpressionFactory[Option[java.sql.Date @@ Q], TOptionDate @@ Q] = taggedOptionTEF
  implicit def timestampTaggedTEF[Q: NotNothing]: TypedExpressionFactory[java.sql.Timestamp @@ Q, TTimestamp @@ Q] = taggedTEF
  implicit def optionTimestampTaggedTEF[Q: NotNothing]: TypedExpressionFactory[Option[java.sql.Timestamp @@ Q], TOptionTimestamp @@ Q] = taggedOptionTEF

  implicit def doubleArrayTaggedTEF[Q: NotNothing]: TypedExpressionFactory[Array[Double] @@ Q, TDoubleArray @@ Q] = taggedTEF
  implicit def intArrayTaggedTEF[Q: NotNothing]: TypedExpressionFactory[Array[Int] @@ Q, TIntArray @@ Q] = taggedTEF
  implicit def longArrayTaggedTEF[Q: NotNothing]: TypedExpressionFactory[Array[Long] @@ Q, TLongArray @@ Q] = taggedTEF
  implicit def stringArrayTaggedTEF[Q: NotNothing]: TypedExpressionFactory[Array[String] @@ Q, TStringArray @@ Q] = taggedTEF

  // =========================== Tagged Numerical Integral ===========================
  implicit def byteTaggedTEF[Q: NotNothing]: TypedExpressionFactory[Byte @@ Q, TByte @@ Q] = taggedTEF
  implicit def optionByteTaggedTEF[Q: NotNothing]: TypedExpressionFactory[Option[Byte @@ Q], TOptionByte @@ Q] = taggedOptionTEF
  implicit def intTaggedTEF[Q: NotNothing]: TypedExpressionFactory[Int @@ Q, TInt @@ Q] = taggedTEF
  implicit def optionIntTaggedTEF[Q: NotNothing]: TypedExpressionFactory[Option[Int @@ Q], TOptionInt @@ Q] = taggedOptionTEF
  implicit def longTaggedTEF[Q: NotNothing]: TypedExpressionFactory[Long @@ Q, TLong @@ Q] = taggedTEF
  implicit def optionLongTaggedTEF[Q: NotNothing]: TypedExpressionFactory[Option[Long @@ Q], TOptionLong @@ Q] = taggedOptionTEF

  // =========================== Tagged Numerical Floating Point ===========================
  implicit def floatTaggedTEF[Q: NotNothing]: TypedExpressionFactory[Float @@ Q, TFloat @@ Q] = taggedTEF
  implicit def optionFloatTaggedTEF[Q: NotNothing]: TypedExpressionFactory[Option[Float @@ Q], TOptionFloat @@ Q] = taggedOptionTEF
  implicit def doubleTaggedTEF[Q: NotNothing]: TypedExpressionFactory[Double @@ Q, TDouble @@ Q] = taggedTEF
  implicit def optionDoubleTaggedTEF[Q: NotNothing]: TypedExpressionFactory[Option[Double @@ Q], TOptionDouble @@ Q] = taggedOptionTEF
  implicit def bigDecimalTaggedTEF[Q: NotNothing]: TypedExpressionFactory[BigDecimal @@ Q, TBigDecimal @@ Q] = taggedTEF
  implicit def optionBigDecimalTaggedTEF[Q: NotNothing]: TypedExpressionFactory[Option[BigDecimal @@ Q], TOptionBigDecimal @@ Q] = taggedOptionTEF

  implicit def stringTaggedToTE[Q: NotNothing](s: String @@ Q): TypedExpression[String @@ Q, TString @@ Q] = stringTaggedTEF.create(s)
  implicit def optionStringTaggedToTE[Q: NotNothing](s: Option[String @@ Q]): TypedExpression[Option[String @@ Q], TOptionString @@ Q]  = optionStringTaggedTEF.create(s)

  implicit def dateToTaggedTE[Q: NotNothing](s: Date @@ Q): TypedExpression[Date @@ Q, TDate @@ Q]  = dateTaggedTEF.create(s)
  implicit def optionDateToTaggedTE[Q: NotNothing](s: Option[Date @@ Q]): TypedExpression[Option[Date @@ Q], TOptionDate @@ Q] = optionDateTaggedTEF.create(s)

  implicit def timestampTaggedToTE[Q: NotNothing](s: Timestamp @@ Q): TypedExpression[Timestamp @@ Q, TTimestamp @@ Q]  = timestampTaggedTEF.create(s)
  implicit def optionTimestampTaggedToTE[Q: NotNothing](s: Option[Timestamp @@ Q]) = optionTimestampTaggedTEF.create(s)

  implicit def booleanTaggedToTE[Q: NotNothing](s: Boolean @@ Q): TypedExpression[Boolean @@ Q, TBoolean @@ Q] =
    subst2[Boolean, TBoolean, TypedExpression, Q](
      PrimitiveTypeSupport.booleanTEF.create(Tag.unwrap(s))
    )
  implicit def optionBooleanTaggedToTE[Q: NotNothing](s: Option[Boolean @@ Q]): TypedExpression[Option[Boolean @@ Q], TOptionBoolean @@ Q] =
    subst2[Boolean, TOptionBoolean, ({type L[a, b] = TypedExpression[Option[a], b]})#L, Q](
      PrimitiveTypeSupport.optionBooleanTEF.create(Tag.unsubst(s))
    )

  implicit def uuidTaggedToTE[Q: NotNothing](s: UUID @@ Q): TypedExpression[UUID @@ Q, TUUID @@ Q] =
    subst2[UUID, TUUID, TypedExpression, Q](
      PrimitiveTypeSupport.uuidTEF.create(Tag.unwrap(s))
    )
  implicit def optionUUIDTaggedToTE[Q: NotNothing](s: Option[UUID @@ Q]): TypedExpression[Option[UUID @@ Q], TOptionUUID @@ Q] =
    subst2[UUID, TOptionUUID, ({type L[a, b] = TypedExpression[Option[a], b]})#L, Q](
      PrimitiveTypeSupport.optionUUIDTEF.create(Tag.unsubst(s))
    )

  implicit def binaryTaggedToTE[Q: NotNothing](s: Array[Byte] @@ Q): TypedExpression[Array[Byte] @@ Q, TByteArray @@ Q] =
    subst2[Array[Byte], TByteArray, TypedExpression, Q](
      PrimitiveTypeSupport.binaryTEF.create(Tag.unwrap(s))
    )
  implicit def optionByteArrayTaggedToTE[Q: NotNothing](s: Option[Array[Byte] @@ Q]): TypedExpression[Option[Array[Byte] @@ Q], TOptionByteArray @@ Q] =
    subst2[Array[Byte], TOptionByteArray, ({type L[a, b] = TypedExpression[Option[a], b]})#L, Q](
      PrimitiveTypeSupport.optionByteArrayTEF.create(Tag.unsubst(s))
    )

  implicit def byteTaggedToTE[Q: NotNothing](f: Byte @@ Q): TypedExpression[Byte @@ Q, TByte @@ Q]  = byteTaggedTEF.create(f)
  implicit def optionByteTaggedToTE[Q: NotNothing](f: Option[Byte @@ Q]): TypedExpression[Option[Byte @@ Q], TOptionByte @@ Q]  = optionByteTaggedTEF.create(f)

  implicit def intTaggedToTE[Q: NotNothing](f: Int @@ Q): TypedExpression[Int @@ Q, TInt @@ Q]  = intTaggedTEF.create(f)
  implicit def optionIntTaggedToTE[Q: NotNothing](f: Option[Int @@ Q]): TypedExpression[Option[Int @@ Q], TOptionInt @@ Q]  = optionIntTaggedTEF.create(f)

  implicit def longTaggedToTE[Q: NotNothing](f: Long @@ Q): TypedExpression[Long @@ Q, TLong @@ Q]  = longTaggedTEF.create(f)
  implicit def optionLongTaggedToTE[Q: NotNothing](f: Option[Long @@ Q]): TypedExpression[Option[Long @@ Q], TOptionLong @@ Q]  = optionLongTaggedTEF.create(f)

  implicit def floatTaggedToTE[Q: NotNothing](f: Float @@ Q): TypedExpression[Float @@ Q, TFloat @@ Q]  = floatTaggedTEF.create(f)
  implicit def optionFloatTaggedToTE[Q: NotNothing](f: Option[Float @@ Q]): TypedExpression[Option[Float @@ Q], TOptionFloat @@ Q]  = optionFloatTaggedTEF.create(f)

  implicit def doubleTaggedToTE[Q: NotNothing](f: Double @@ Q): TypedExpression[Double @@ Q, TDouble @@ Q]  = doubleTaggedTEF.create(f)
  implicit def optionDoubleTaggedToTE[Q: NotNothing](f: Option[Double @@ Q]): TypedExpression[Option[Double @@ Q], TOptionDouble @@ Q]  = optionDoubleTaggedTEF.create(f)

  implicit def bigDecimalTaggedToTE[Q: NotNothing](f: BigDecimal @@ Q): TypedExpression[BigDecimal @@ Q, TBigDecimal @@ Q]  = bigDecimalTaggedTEF.create(f)
  implicit def optionBigDecimalTaggedToTE[Q: NotNothing](f: Option[BigDecimal @@ Q]): TypedExpression[Option[BigDecimal @@ Q], TOptionBigDecimal @@ Q]  = optionBigDecimalTaggedTEF.create(f)

  implicit def doubleArrayTaggedToTE[Q: NotNothing](f : Array[Double] @@ Q): TypedExpression[Array[Double] @@ Q, TDoubleArray @@ Q]  = doubleArrayTaggedTEF.create(f)
  implicit def intArrayTaggedToTE[Q: NotNothing](f : Array[Int] @@ Q): TypedExpression[Array[Int] @@ Q, TIntArray @@ Q]  = intArrayTaggedTEF.create(f)
  implicit def longArrayTaggedToTE[Q: NotNothing](f : Array[Long] @@ Q): TypedExpression[Array[Long] @@ Q, TLongArray @@ Q]  = longArrayTaggedTEF.create(f)
  implicit def stringArrayTaggedToTE[Q: NotNothing](f: Array[String] @@ Q): TypedExpression[Array[String] @@ Q, TStringArray @@ Q]  = stringArrayTaggedTEF.create(f)

  implicit def queryStringToTE(q: Query[String]) =
    new QueryValueExpressionNode[String, TString](q.copy(false, Nil).ast, stringTEF.createOutMapper)
  implicit def queryOptionStringToTE(q: Query[Option[String]]) =
    new QueryValueExpressionNode[Option[String], TOptionString](q.copy(false, Nil).ast, optionStringTEF.createOutMapper)
  implicit def queryStringGroupedToTE(q: Query[Group[String]]) =
    new QueryValueExpressionNode[String, TString](q.copy(false, Nil).ast, stringTEF.createOutMapper)
  implicit def queryOptionStringGroupedToTE(q: Query[Group[Option[String]]]) =
    new QueryValueExpressionNode[Option[String], TOptionString](q.copy(false, Nil).ast, optionStringTEF.createOutMapper)
  implicit def queryStringMeasuredToTE(q: Query[Measures[String]]) =
    new QueryValueExpressionNode[String, TString](q.copy(false, Nil).ast, stringTEF.createOutMapper)
  implicit def queryOptionStringMeasuredToTE(q: Query[Measures[Option[String]]]) =
    new QueryValueExpressionNode[Option[String], TOptionString](q.copy(false, Nil).ast, optionStringTEF.createOutMapper)

  implicit def queryDateToTE(q: Query[Date]) =
    new QueryValueExpressionNode[Date, TDate](q.copy(false, Nil).ast, dateTEF.createOutMapper)
  implicit def queryOptionDateToTE(q: Query[Option[Date]]) =
    new QueryValueExpressionNode[Option[Date], TOptionDate](q.copy(false, Nil).ast, optionDateTEF.createOutMapper)
  implicit def queryDateGroupedToTE(q: Query[Group[Date]]) =
    new QueryValueExpressionNode[Date, TDate](q.copy(false, Nil).ast, dateTEF.createOutMapper)
  implicit def queryOptionDateGroupedToTE(q: Query[Group[Option[Date]]]) =
    new QueryValueExpressionNode[Option[Date], TOptionDate](q.copy(false, Nil).ast, optionDateTEF.createOutMapper)
  implicit def queryDateMeasuredToTE(q: Query[Measures[Date]]) =
    new QueryValueExpressionNode[Date, TDate](q.copy(false, Nil).ast, dateTEF.createOutMapper)
  implicit def queryOptionDateMeasuredToTE(q: Query[Measures[Option[Date]]]) =
    new QueryValueExpressionNode[Option[Date], TOptionDate](q.copy(false, Nil).ast, optionDateTEF.createOutMapper)

  implicit def queryTimestampToTE(q: Query[Timestamp]) =
    new QueryValueExpressionNode[Timestamp, TTimestamp](q.copy(false, Nil).ast, timestampTEF.createOutMapper)
  implicit def queryOptionTimestampToTE(q: Query[Option[Timestamp]]) =
    new QueryValueExpressionNode[Option[Timestamp], TOptionTimestamp](q.copy(false, Nil).ast, optionTimestampTEF.createOutMapper)
  implicit def queryTimestampGroupedToTE(q: Query[Group[Timestamp]]) =
    new QueryValueExpressionNode[Timestamp, TTimestamp](q.copy(false, Nil).ast, timestampTEF.createOutMapper)
  implicit def queryOptionTimestampGroupedToTE(q: Query[Group[Option[Timestamp]]]) =
    new QueryValueExpressionNode[Option[Timestamp], TOptionTimestamp](q.copy(false, Nil).ast, optionTimestampTEF.createOutMapper)
  implicit def queryTimestampMeasuredToTE(q: Query[Measures[Timestamp]]) =
    new QueryValueExpressionNode[Timestamp, TTimestamp](q.copy(false, Nil).ast, timestampTEF.createOutMapper)
  implicit def queryOptionTimestampMeasuredToTE(q: Query[Measures[Option[Timestamp]]]) =
    new QueryValueExpressionNode[Option[Timestamp], TOptionTimestamp](q.copy(false, Nil).ast, optionTimestampTEF.createOutMapper)

  implicit def queryBooleanToTE(q: Query[Boolean]) =
    new QueryValueExpressionNode[Boolean, TBoolean](q.copy(false, Nil).ast, PrimitiveTypeSupport.booleanTEF.createOutMapper)
  implicit def queryOptionBooleanToTE(q: Query[Option[Boolean]]) =
    new QueryValueExpressionNode[Option[Boolean], TOptionBoolean](q.copy(false, Nil).ast, PrimitiveTypeSupport.optionBooleanTEF.createOutMapper)

  implicit def queryUUIDToTE(q: Query[UUID]) =
    new QueryValueExpressionNode[UUID, TUUID](q.copy(false, Nil).ast, PrimitiveTypeSupport.uuidTEF.createOutMapper)
  implicit def queryOptionUUIDToTE(q: Query[Option[UUID]]) =
    new QueryValueExpressionNode[Option[UUID], TOptionUUID](q.copy(false, Nil).ast, PrimitiveTypeSupport.optionUUIDTEF.createOutMapper)

  implicit def queryByteArrayToTE(q: Query[Array[Byte]]) =
    new QueryValueExpressionNode[Array[Byte], TByteArray](q.copy(false, Nil).ast, PrimitiveTypeSupport.binaryTEF.createOutMapper)
  implicit def queryOptionByteArrayToTE(q: Query[Option[Array[Byte]]]) =
    new QueryValueExpressionNode[Option[Array[Byte]], TOptionByteArray](q.copy(false, Nil).ast, PrimitiveTypeSupport.optionByteArrayTEF.createOutMapper)

  implicit def queryByteToTE(q: Query[Byte]) =
    new QueryValueExpressionNode[Byte, TByte](q.copy(false, Nil).ast, byteTEF.createOutMapper)
  implicit def queryOptionByteToTE(q: Query[Option[Byte]]) =
    new QueryValueExpressionNode[Option[Byte], TOptionByte](q.copy(false, Nil).ast, optionByteTEF.createOutMapper)
  implicit def queryByteGroupedToTE(q: Query[Group[Byte]]) =
    new QueryValueExpressionNode[Byte, TByte](q.copy(false, Nil).ast, byteTEF.createOutMapper)
  implicit def queryOptionByteGroupedToTE(q: Query[Group[Option[Byte]]]) =
    new QueryValueExpressionNode[Option[Byte], TOptionByte](q.copy(false, Nil).ast, optionByteTEF.createOutMapper)
  implicit def queryByteMeasuredToTE(q: Query[Measures[Byte]]) =
    new QueryValueExpressionNode[Byte, TByte](q.copy(false, Nil).ast, byteTEF.createOutMapper)
  implicit def queryOptionByteMeasuredToTE(q: Query[Measures[Option[Byte]]]) =
    new QueryValueExpressionNode[Option[Byte], TOptionByte](q.copy(false, Nil).ast, optionByteTEF.createOutMapper)

  implicit def queryIntToTE(q: Query[Int]) =
    new QueryValueExpressionNode[Int, TInt](q.copy(false, Nil).ast, intTEF.createOutMapper)
  implicit def queryOptionIntToTE(q: Query[Option[Int]]) =
    new QueryValueExpressionNode[Option[Int], TOptionInt](q.copy(false, Nil).ast, optionIntTEF.createOutMapper)
  implicit def queryIntGroupedToTE(q: Query[Group[Int]]) =
    new QueryValueExpressionNode[Int, TInt](q.copy(false, Nil).ast, intTEF.createOutMapper)
  implicit def queryOptionIntGroupedToTE(q: Query[Group[Option[Int]]]) =
    new QueryValueExpressionNode[Option[Int], TOptionInt](q.copy(false, Nil).ast, optionIntTEF.createOutMapper)
  implicit def queryIntMeasuredToTE(q: Query[Measures[Int]]) =
    new QueryValueExpressionNode[Int, TInt](q.copy(false, Nil).ast, intTEF.createOutMapper)
  implicit def queryOptionIntMeasuredToTE(q: Query[Measures[Option[Int]]]) =
    new QueryValueExpressionNode[Option[Int], TOptionInt](q.copy(false, Nil).ast, optionIntTEF.createOutMapper)

  implicit def queryLongToTE(q: Query[Long]) =
    new QueryValueExpressionNode[Long, TLong](q.copy(false, Nil).ast, longTEF.createOutMapper)
  implicit def queryOptionLongToTE(q: Query[Option[Long]]) =
    new QueryValueExpressionNode[Option[Long], TOptionLong](q.copy(false, Nil).ast, optionLongTEF.createOutMapper)
  implicit def queryLongGroupedToTE(q: Query[Group[Long]]) =
    new QueryValueExpressionNode[Long, TLong](q.copy(false, Nil).ast, longTEF.createOutMapper)
  implicit def queryOptionLongGroupedToTE(q: Query[Group[Option[Long]]]) =
    new QueryValueExpressionNode[Option[Long], TOptionLong](q.copy(false, Nil).ast, optionLongTEF.createOutMapper)
  implicit def queryLongMeasuredToTE(q: Query[Measures[Long]]) =
    new QueryValueExpressionNode[Long, TLong](q.copy(false, Nil).ast, longTEF.createOutMapper)
  implicit def queryOptionLongMeasuredToTE(q: Query[Measures[Option[Long]]]) =
    new QueryValueExpressionNode[Option[Long], TOptionLong](q.copy(false, Nil).ast, optionLongTEF.createOutMapper)

  implicit def queryFloatToTE(q: Query[Float]) =
    new QueryValueExpressionNode[Float, TFloat](q.copy(false, Nil).ast, floatTEF.createOutMapper)
  implicit def queryOptionFloatToTE(q: Query[Option[Float]]) =
    new QueryValueExpressionNode[Option[Float], TOptionFloat](q.copy(false, Nil).ast, optionFloatTEF.createOutMapper)
  implicit def queryFloatGroupedToTE(q: Query[Group[Float]]) =
    new QueryValueExpressionNode[Float, TFloat](q.copy(false, Nil).ast, floatTEF.createOutMapper)
  implicit def queryOptionFloatGroupedToTE(q: Query[Group[Option[Float]]]) =
    new QueryValueExpressionNode[Option[Float], TOptionFloat](q.copy(false, Nil).ast, optionFloatTEF.createOutMapper)
  implicit def queryFloatMeasuredToTE(q: Query[Measures[Float]]) =
    new QueryValueExpressionNode[Float, TFloat](q.copy(false, Nil).ast, floatTEF.createOutMapper)
  implicit def queryOptionFloatMeasuredToTE(q: Query[Measures[Option[Float]]]) =
    new QueryValueExpressionNode[Option[Float], TOptionFloat](q.copy(false, Nil).ast, optionFloatTEF.createOutMapper)

  implicit def queryDoubleToTE(q: Query[Double]) =
    new QueryValueExpressionNode[Double, TDouble](q.copy(false, Nil).ast, doubleTEF.createOutMapper)
  implicit def queryOptionDoubleToTE(q: Query[Option[Double]]) =
    new QueryValueExpressionNode[Option[Double], TOptionDouble](q.copy(false, Nil).ast, optionDoubleTEF.createOutMapper)
  implicit def queryDoubleGroupedToTE(q: Query[Group[Double]]) =
    new QueryValueExpressionNode[Double, TDouble](q.copy(false, Nil).ast, doubleTEF.createOutMapper)
  implicit def queryOptionDoubleGroupedToTE(q: Query[Group[Option[Double]]]) =
    new QueryValueExpressionNode[Option[Double], TOptionDouble](q.copy(false, Nil).ast, optionDoubleTEF.createOutMapper)
  implicit def queryDoubleMeasuredToTE(q: Query[Measures[Double]]) =
    new QueryValueExpressionNode[Double, TDouble](q.copy(false, Nil).ast, doubleTEF.createOutMapper)
  implicit def queryOptionDoubleMeasuredToTE(q: Query[Measures[Option[Double]]]) =
    new QueryValueExpressionNode[Option[Double], TOptionDouble](q.copy(false, Nil).ast, optionDoubleTEF.createOutMapper)

  implicit def queryBigDecimalToTE(q: Query[BigDecimal]) =
    new QueryValueExpressionNode[BigDecimal, TBigDecimal](q.copy(false, Nil).ast, bigDecimalTEF.createOutMapper)
  implicit def queryOptionBigDecimalToTE(q: Query[Option[BigDecimal]]) =
    new QueryValueExpressionNode[Option[BigDecimal], TOptionBigDecimal](q.copy(false, Nil).ast, optionBigDecimalTEF.createOutMapper)
  implicit def queryBigDecimalGroupedToTE(q: Query[Group[BigDecimal]]) =
    new QueryValueExpressionNode[BigDecimal, TBigDecimal](q.copy(false, Nil).ast, bigDecimalTEF.createOutMapper)
  implicit def queryOptionBigDecimalGroupedToTE(q: Query[Group[Option[BigDecimal]]]) =
    new QueryValueExpressionNode[Option[BigDecimal], TOptionBigDecimal](q.copy(false, Nil).ast, optionBigDecimalTEF.createOutMapper)
  implicit def queryBigDecimalMeasuredToTE(q: Query[Measures[BigDecimal]]) =
    new QueryValueExpressionNode[BigDecimal, TBigDecimal](q.copy(false, Nil).ast, bigDecimalTEF.createOutMapper)
  implicit def queryOptionBigDecimalMeasuredToTE(q: Query[Measures[Option[BigDecimal]]]) =
    new QueryValueExpressionNode[Option[BigDecimal], TOptionBigDecimal](q.copy(false, Nil).ast, optionBigDecimalTEF.createOutMapper)

}
