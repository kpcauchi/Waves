package com.wavesplatform.state2.reader

import cats.data.{NonEmptyList => NEL}
import cats.implicits._
import cats.kernel.Monoid
import com.wavesplatform.state2._
import monix.eval.Coeval
import scorex.account.{Address, Alias}
import scorex.transaction.Transaction
import scorex.transaction.assets.IssueTransaction
import scorex.transaction.lease.LeaseTransaction

class CompositeStateReader private(inner: SnapshotStateReader, blockDiff: BlockDiff) extends SnapshotStateReader {

  override def assetDescription(id: ByteStr) = {
    inner
      .assetDescription(id).orElse(blockDiff.txsDiff.transactions.get(id).collectFirst {
        case (_, it: IssueTransaction, _) => AssetDescription(it.sender, it.name, it.decimals, it.reissuable)
      })
      .map(z => blockDiff.txsDiff.issuedAssets.get(id).fold(z)(r => z.copy(reissuable = r.isReissuable)))

  }

  override def leaseOverflows = {
    val innerAddressesWithOverflows = inner.leaseOverflows.keys
    val addressesWithNewLeases = txDiff.portfolios.collect {
      case (address, portfolio) if portfolio.leaseInfo.leaseOut > 0 => address
    }

    (innerAddressesWithOverflows ++ addressesWithNewLeases)
      .map(a => a -> (wavesBalance(a).regularBalance - leaseInfo(a).leaseOut))
      .filter { case (a, overflow) => overflow < 0 }
      .toMap
  }

  override def leasesOf(address: Address) = {
    val innerLeases = inner.leasesOf(address)
    val leaseChanges = txDiff.leaseState.flatMap {
      case (id, false) => Some(id -> innerLeases(id).copy(isActive = false))
      case (id, true) =>
        txDiff.transactions.get(id) match {
          case Some((h, lt: LeaseTransaction, _)) => Some(id -> LeaseDetails(lt.sender, lt.recipient, h, lt.amount, true))
          case _ => None
        }
    }

    innerLeases ++ leaseChanges
  }

  override def nonZeroLeaseBalances = inner.nonZeroLeaseBalances ++ txDiff.portfolios.collect {
    case (addr, p) if p.leaseInfo != LeaseInfo.empty => addr -> p.leaseInfo
  }

  override def leaseDetails(leaseId: ByteStr) = blockDiff.txsDiff.transactions.get(leaseId) match {
    case Some((h, l: LeaseTransaction, _)) => Some(LeaseDetails(l.sender, l.recipient, h, l.amount, txDiff.leaseState.getOrElse(leaseId, false)))
    case _ => inner.leaseDetails(leaseId)
  }

  override def leaseInfo(a: Address) = Monoid.combine(blockDiff.txsDiff.portfolios.get(a).fold(Monoid.empty[LeaseInfo])(p => p.leaseInfo), inner.leaseInfo(a))

  private val txDiff = blockDiff.txsDiff

  override def transactionInfo(id: ByteStr): Option[(Int, Option[Transaction])] =
    txDiff.transactions.get(id)
      .map(t => (t._1, Some(t._2)))
      .orElse(inner.transactionInfo(id))

  override def height: Int = inner.height + blockDiff.heightDiff

  override def accountTransactionIds(a: Address, limit: Int): Seq[ByteStr] = {
    val fromDiff = txDiff.accountTransactionIds.get(a).orEmpty
    if (fromDiff.lengthCompare(limit) >= 0) {
      fromDiff.take(limit)
    } else {
      fromDiff ++ inner.accountTransactionIds(a, limit - fromDiff.size) // fresh head ++ stale tail
    }
  }

  override def wavesBalance(a: Address) = {
    val innerBalance = inner.wavesBalance(a)
    txDiff.portfolios.get(a).fold(innerBalance) { p =>
      innerBalance.copy(
        regularBalance = safeSum(innerBalance.regularBalance, p.balance),
        effectiveBalance = safeSum(innerBalance.effectiveBalance, p.leaseInfo.leaseIn) - p.leaseInfo.leaseOut)
    }
  }

  override def assetBalance(a: Address) =
    inner.assetBalance(a) ++ blockDiff.snapshots.get(a).fold(Map.empty[ByteStr, Long])(_.assetBalances)

  override def snapshotAtHeight(acc: Address, h: Int): Option[Snapshot] = ???

  override def paymentTransactionIdByHash(hash: ByteStr): Option[ByteStr]
  = blockDiff.txsDiff.paymentTransactionIdsByHashes.get(hash)
    .orElse(inner.paymentTransactionIdByHash(hash))

  override def aliasesOfAddress(a: Address): Seq[Alias] =
    txDiff.aliases.filter(_._2 == a).keys.toSeq ++ inner.aliasesOfAddress(a)

  override def resolveAlias(a: Alias): Option[Address] = txDiff.aliases.get(a).orElse(inner.resolveAlias(a))

  override def activeLeases: Seq[ByteStr] = {
    blockDiff.txsDiff.leaseState.collect { case (id, isActive) if isActive => id }.toSeq ++ inner.activeLeases
  }

  override def lastUpdateHeight(acc: Address): Option[Int] = ???

  override def containsTransaction(id: ByteStr): Boolean = blockDiff.txsDiff.transactions.contains(id) || inner.containsTransaction(id)

  override def filledVolumeAndFee(orderId: ByteStr): OrderFillInfo =
    blockDiff.txsDiff.orderFills.get(orderId).orEmpty.combine(inner.filledVolumeAndFee(orderId))
}

object CompositeStateReader {
  def composite(blockDiff: BlockDiff, inner: SnapshotStateReader): SnapshotStateReader = new CompositeStateReader(inner, blockDiff)

  def composite(blockDiff: Seq[BlockDiff], inner: SnapshotStateReader): SnapshotStateReader = blockDiff match {
    case (x :: xs) => composite(x, composite(xs, inner))
    case _ => inner
  }

  def composite(blockDiffs: NEL[BlockDiff], inner: SnapshotStateReader): SnapshotStateReader = blockDiffs.tail match {
    case (x :: xs) => composite(blockDiffs.head, composite(NEL(x, xs), inner))
    case Nil => composite(blockDiffs.head, inner)
  }

  // fresh head
  def composite(blockDiff: Coeval[BlockDiff], inner: Coeval[SnapshotStateReader]): Coeval[SnapshotStateReader] = for {
    i <- inner
    b <- blockDiff
  } yield composite(b, i)
}
