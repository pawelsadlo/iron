package io.github.iltotore.iron

import com.github.plokhotnyuk.jsoniter_scala.core.*
import com.github.plokhotnyuk.jsoniter_scala.macros.*

object jsoniter:

  /**
   * Creates a [[JsonValueCodec]] for refined types
   *
   * @param constraint the [[Constraint]] implementation to test the decoded value.
   */
  inline given makeCodec[A, B](using
      inline constraint: Constraint[A, B],
      inline config: CodecMakerConfig = CodecMakerConfig
  ): JsonValueCodec[A :| B] =
    IronJsonValueCodec(JsonCodecMaker.make[A](config), (x: A) => constraint.test(x), constraint.message)

  // Introduced to resolve "New anonymous class definition will be duplicated at each inline site".
  private final class IronJsonValueCodec[A, B] private (codec: JsonValueCodec[A], constraintTest: A => Boolean, constraintMessage: String)
      extends JsonValueCodec[A :| B]:
    override def decodeValue(in: JsonReader, default: A :| B): A :| B =
      val decoded = codec.decodeValue(in, default)
      if constraintTest(decoded) then decoded.asInstanceOf[A :| B]
      else in.decodeError(constraintMessage)

    override def encodeValue(x: A :| B, out: JsonWriter): Unit =
      codec.encodeValue(x, out)

    override def nullValue: A :| B = null.asInstanceOf[A :| B]

  end IronJsonValueCodec

  /**
   * Internal API, should not be used in user code.
   */
  object IronJsonValueCodec:
    /**
     * Internal API, should not be used in user code.
     */
    def apply[A, B](codec: JsonValueCodec[A], constraintTest: A => Boolean, constraintMessage: String): JsonValueCodec[A :| B] =
      new IronJsonValueCodec(codec, constraintTest, constraintMessage)
