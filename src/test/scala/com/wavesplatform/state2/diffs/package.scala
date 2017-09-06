package com.wavesplatform.state2

import com.wavesplatform.features.FeatureProvider
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state2.reader.CompositeStateReader.composite
import com.wavesplatform.state2.reader.{CompositeStateReader, SnapshotStateReader}
import scorex.block.Block
import scorex.settings.TestFunctionalitySettings
import scorex.transaction.{History, ValidationError}

package object diffs {
  def newState(): SnapshotStateReader with StateWriter = ???

  def newHistory(): History with FeatureProvider = ???

  val ENOUGH_AMT: Long = Long.MaxValue / 3

  def assertDiffEi(preconditions: Seq[Block], block: Block, fs: FunctionalitySettings = TestFunctionalitySettings.Enabled)(assertion: Either[ValidationError, BlockDiff] => Unit): Unit = {
    val fp = newHistory()
    val state = newState()
    val differ: (SnapshotStateReader, Block) => Either[ValidationError, BlockDiff] = (s, b) => BlockDiffer.fromBlock(fs, fp, s, None, b)

    preconditions.foreach { precondition =>
      val preconditionDiff = differ(state, precondition).explicitGet()
      state.append(preconditionDiff, ???)
    }
    val totalDiff1 = differ(state, block)
    assertion(totalDiff1)

    val preconditionDiff = BlockDiffer.unsafeDiffMany(fs, fp, newState(), None, 6)(preconditions)
    val compositeState = composite(preconditionDiff, newState())
    val totalDiff2 = differ(compositeState, block)
    assertion(totalDiff2)
  }


  def assertDiffEiWithPrev(preconditions: Seq[Block], block: Block, fs: FunctionalitySettings = TestFunctionalitySettings.Enabled)(assertion: (BlockDiff, SnapshotStateReader) => Unit): Unit = {
    val differ: (SnapshotStateReader, (Option[Block], Block)) => Either[ValidationError, BlockDiff] = {
      case (s, (prev, b)) => BlockDiffer.fromBlock(fs, ???, s, prev, b)
    }

    val state = newState()

    zipWithPrev(preconditions).foreach { wp =>
      val preconditionDiff = differ(state, wp).explicitGet()
      state.append(preconditionDiff, ???)
    }

    val totalDiff1 = differ(state, (preconditions.lastOption, block)).explicitGet()
    state.append(totalDiff1, ???)
    assertion(totalDiff1, state)

    val preconditionDiff = BlockDiffer.unsafeDiffMany(fs, ???, newState(), None, ???)(preconditions)
    val compositeState = CompositeStateReader.composite(preconditionDiff, newState)
    val totalDiff2 = differ(compositeState, (preconditions.lastOption, block)).explicitGet()
    assertion(totalDiff2, CompositeStateReader.composite(totalDiff2, compositeState))

    assert(totalDiff1 == totalDiff2)
  }

  def assertDiffAndState(preconditions: Seq[Block], block: Block, fs: FunctionalitySettings = TestFunctionalitySettings.Enabled)(assertion: (BlockDiff, SnapshotStateReader) => Unit): Unit = {
    val fp = newHistory()
    val state = newState()

    val differ: (SnapshotStateReader, Block) => Either[ValidationError, BlockDiff] = (s, b) => BlockDiffer.fromBlock(fs, fp, s, None, b)

    preconditions.foreach { precondition =>
      val preconditionDiff = differ(state, precondition).explicitGet()
      state.append(preconditionDiff, ???)
    }
    val totalDiff1 = differ(state, block).explicitGet()
    state.append(totalDiff1, ???)
    assertion(totalDiff1, state)

    val preconditionDiff = BlockDiffer.unsafeDiffMany(fs, fp, newState(), None, 7)(preconditions)
    val compositeState = composite(preconditionDiff, newState())
    val totalDiff2 = differ(compositeState, block).explicitGet()
    assertion(totalDiff2, composite(totalDiff2, compositeState))
  }

  def produce(errorMessage: String): ProduceError = new ProduceError(errorMessage)

  def zipWithPrev[A](seq: Seq[A]): Seq[(Option[A], A)] = {
    seq.zipWithIndex.map { case ((a, i)) => (if (i == 0) None else Some(seq(i - 1)), a) }
  }
}
