package com.wavesplatform.state2.reader

import com.wavesplatform.state2._
import scorex.account.{Address, AddressOrAlias, Alias}
import scorex.transaction.ValidationError.AliasNotExists
import scorex.transaction._
import scorex.transaction.assets.IssueTransaction
import scorex.utils.ScorexLogging

import scala.util.Right

trait SnapshotStateReader {
  def transactionInfo(id: ByteStr): Option[(Int, Option[Transaction])]

  def containsTransaction(id: ByteStr): Boolean

  def assetDescription(id: ByteStr): Option[AssetDescription]

  def wavesBalance(a: Address): WavesBalance

  def assetBalance(a: Address): Map[ByteStr, Long]

  def nonZeroLeaseBalances: Map[Address, LeaseInfo]

  def height: Int

  def accountTransactionIds(a: Address, limit: Int): Seq[ByteStr]

  def paymentTransactionIdByHash(hash: ByteStr): Option[ByteStr]

  def aliasesOfAddress(a: Address): Seq[Alias]

  def resolveAlias(a: Alias): Option[Address]

  def leaseDetails(leaseId: ByteStr): Option[LeaseDetails]
  def leaseInfo(a: Address): LeaseInfo

  def activeLeases: Seq[ByteStr]
  def leaseOverflows: Map[Address, Long]
  def leasesOf(address: Address): Map[ByteStr, LeaseDetails]

  def lastUpdateHeight(acc: Address): Option[Int]

  def snapshotAtHeight(acc: Address, h: Int): Option[Snapshot]

  def filledVolumeAndFee(orderId: ByteStr): OrderFillInfo
}

object SnapshotStateReader {

  implicit class StateReaderExt(s: SnapshotStateReader) extends ScorexLogging {
    def assetDistribution(assetId: ByteStr): Map[Address, Long] = ???

    def resolveAliasEi[T <: Transaction](aoa: AddressOrAlias): Either[ValidationError, Address] = {
      aoa match {
        case a: Address => Right(a)
        case a: Alias => s.resolveAlias(a) match {
          case None => Left(AliasNotExists(a))
          case Some(acc) => Right(acc)
        }
      }
    }

    def included(signature: ByteStr): Option[Int] = s.transactionInfo(signature).map(_._1)

    def accountTransactions(account: Address, limit: Int): Seq[(Int, _ <: Transaction)] = Seq.empty

    def getAccountBalance(account: Address): Map[AssetId, (Long, Boolean, Long, IssueTransaction)] = Map.empty

    def assetDistribution(assetId: Array[Byte]): Map[String, Long] =
      s.assetDistribution(ByteStr(assetId))
        .map { case (acc, amt) => (acc.address, amt) }

    def balanceWithConfirmations(acc: Address, confirmations: Int): Long = ???
    def effectiveBalanceAtHeightWithConfirmations(acc: Address, height: Int, confirmations: Int): Long = ???

    def balanceAtHeight(acc: Address, height: Int): Long = ???

    def accountPortfoliosHash: Int = 0
  }

}
