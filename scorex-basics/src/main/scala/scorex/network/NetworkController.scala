package scorex.network

import java.net.{InetAddress, InetSocketAddress}

import akka.actor._
import akka.io.Tcp._
import akka.io.{IO, Tcp}
import scorex.app.Application
import scorex.network.message.{MessageSpec, Message}
import scorex.utils.ScorexLogging

import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util.{Failure, Success}


//must be singleton
class NetworkController(application: Application) extends Actor with ScorexLogging {

  import NetworkController._

  private implicit val system = context.system

  private lazy val settings = application.settings
  private lazy val peerManager = application.peerManager

  private val connectedPeers = mutable.Map[InetSocketAddress, ConnectedPeer]()
  private val connectingPeers = mutable.Buffer[InetSocketAddress]()

  private val messageHandlers = mutable.Map[Seq[Message.MessageCode], ActorRef]()

  IO(Tcp) ! Bind(self, new InetSocketAddress(InetAddress.getByName(settings.bindAddress), settings.Port))


  override def receive = {
    case b@Bound(localAddress) =>
      log.info("Successfully bound to the port " + settings.Port)
      context.system.scheduler.schedule(200.millis, 3.seconds)(self ! CheckPeers)

    case CommandFailed(_: Bind) =>
      log.error("Network port " + settings.Port + " already in use!")
      context stop self
      application.stopAll()

    case CheckPeers =>
      if (connectedPeers.size < settings.maxConnections) {
        peerManager.randomPeer() match {
          case Some(peer) =>
            if (!connectedPeers.contains(peer) && !connectingPeers.contains(peer)) {
              connectingPeers += peer
              val connTimeout = Some(new FiniteDuration(settings.connectionTimeout, SECONDS))
              IO(Tcp) ! Connect(peer, timeout = connTimeout)
            }

          case None =>
        }
      }

    case c@Connected(remote, local) =>
      log.info(s"Connected to $remote")
      connectingPeers -= remote
      val connection = sender()
      val handler = context.actorOf(Props(classOf[PeerConnectionHandler], application, connection, remote))
      connection ! Register(handler)
      connectedPeers += remote -> ConnectedPeer(remote, handler)
      peerManager.peerConnected(remote)

    case CommandFailed(c: Connect) =>
      log.info("Failed to connect to : " + c.remoteAddress)
      connectedPeers -= c.remoteAddress
      peerManager.peerDisconnected(c.remoteAddress)

    case CommandFailed(cmd: Tcp.Command) =>
      log.info("Failed to execute command : " + cmd)

    case ShutdownNetwork =>
      log.info("Going to shutdown all connections & unbind port")
      connectedPeers.values.foreach(_.handlerRef ! PeerConnectionHandler.CloseConnection)
      self ! Unbind
      context stop self

    case PeerDisconnected(remote) =>
      connectedPeers -= remote
      peerManager.peerDisconnected(remote)


    case RegisterMessagesHandler(specs, handler) =>
      messageHandlers += specs.map(_.messageCode) -> handler

    case Message(spec, Left(msgBytes), Some(remote)) =>
      val msgId = spec.messageCode

      spec.deserializeData(msgBytes) match {
        case Success(content) =>
          messageHandlers.find(_._1.contains(msgId)).map(_._2) match {
            case Some(handler) =>
              handler ! DataFromPeer(content, remote)

            case None => //todo: ???
          }
        case Failure(e) =>
          //todo: ban peer
      }

    case SendToNetwork(message, sendingStrategy) =>
      sendingStrategy.choose(connectedPeers.values.toSeq).foreach(_.handlerRef ! message)


  /*  case AskForPeers =>
      self ! SendMessageToRandomPeer(BasicMessagesRepo.GetPeersMessage)


    case BroadcastMessage(message, exceptOf) =>
      log.info(s"Broadcasting message $message")
      connectedPeers.foreach { case (remote, PeerData(handler, _)) =>
        if (!exceptOf.contains(remote)) handler ! message
      }
      log.info("Broadcasting end")

    case SendMessageToBestPeer(message) =>
      maxScoreHandler().foreach { handler =>
        log.info(s"Sending $message to a best peer ${handler.path}")
        handler ! message
      }

    case SendMessageToRandomPeer(message) =>
      val handlers = connectedPeers.values.toList
      if (handlers.nonEmpty) {
        val randomHandler = handlers(Random.nextInt(handlers.size)).handler
        randomHandler ! message
      } */

    case GetPeers => sender() ! connectedPeers.values.toSeq

    /* case GetMaxChainScore =>
      sender() ! BlockGenerator.MaxChainScore(maxPeerScore())

    case NewBlock(block, Some(sndr)) =>
      application.blockchainSyncer ! NewBlock(block, Some(sndr))
      self ! BroadcastMessage(BlockMessage(block), List(sndr))

    case UpdateBlockchainScore(remote, height, score) => updateScore(remote, height, score)
    */

    case nonsense: Any => log.warn(s"NetworkController: got something strange $nonsense")
  }
}

object NetworkController {
  case class RegisterMessagesHandler(specs: Seq[MessageSpec[_]], handler: ActorRef)
  case class DataFromPeer[V](data:V, source:ConnectedPeer)
  case class SendToNetwork(message: Message[_], sendingStrategy: SendingStrategy)


  private case object CheckPeers

//  private case object AskForPeers

  case object ShutdownNetwork

  case object GetPeers

  // case object GetMaxBlockchainScore

  case class PeerDisconnected(address: InetSocketAddress)

  //case class UpdateBlockchainScore(remote: InetSocketAddress, height: Int, score: BigInt)

  /*
  case class SendMessageToBestPeer(msg: message.Message[_])

  case class SendMessageToRandomPeer(msg: message.Message[_])

  case class BroadcastMessage(msg: message.Message[_], exceptOf: Seq[InetSocketAddress] = List())
  */
}

/*
case class OutcomingMessagingRule(sendingStrategy: SendingStrategy,
                                  interaction: Interaction,
                                  scheduler: Option[(FiniteDuration, FiniteDuration)])
*/

//initial delay, delay

//get peers

/*
trait NetworkApplicationLogic {
  val rules: Seq[OutcomingMessagingRule]
}

case class PeersLogic(peerManager: PeerManager) extends NetworkApplicationLogic {

  private object peersExchange extends OutcomingMessagingRule(
    SendToRandom,
    PeersInteraction(peerManager),
    Some(1.second -> 3.seconds)
  )

  override val rules: Seq[OutcomingMessagingRule] = Seq(peersExchange)
}


trait BlockchainLogic extends NetworkApplicationLogic {

  object newBlock extends OutcomingMessagingRule(Broadcast, BlockInteraction, None)

  //object bestExtension extends OutcomingMessagingRule(BestPeer, SignaturesInteraction, None)

  override val rules = super.rules ++ Seq()
} */
