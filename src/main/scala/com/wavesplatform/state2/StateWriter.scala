package com.wavesplatform.state2

import com.wavesplatform.state2.StateWriter.Status
import monix.reactive.Observable
import scorex.block.Block
import scorex.transaction.Transaction

trait StateWriter {
  def append(blockDiff: BlockDiff, block: Block): Unit
  def rollbackTo(targetBlockId: ByteStr): Seq[Transaction]
  def status: Observable[Status]
}

object StateWriter {
  case class Status(height: Int, lastUpdated: Long)
}