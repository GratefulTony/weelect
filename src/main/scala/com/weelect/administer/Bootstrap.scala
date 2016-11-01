package com.weelect.administer

import java.io.File
import java.util.Date
import com.weelect.audit.ElectionState
import org.bitcoinj.core._
import org.bitcoinj.kits.WalletAppKit
import org.bitcoinj.net.discovery.DnsDiscovery
import org.bitcoinj.params.MainNetParams
import org.bitcoinj.script.{ScriptBuilder, ScriptOpCodes}
import org.bitcoinj.store.SPVBlockStore
import org.bitcoinj.utils.BriefLogFormatter

import scala.collection.JavaConverters._

/**
 * Created by t on 1/28/16.
 */

//debug coordination addr: 1DXqsqqLh5Bej6pKrXw9aQxWXoEC5Qrbnw
//debug witness addr: 14rQHjHEcjNLd2mmq1jBTrJ9yVZ2xS6kax

object Bootstrap extends App {

  val usage = "\n\nUSAGE: \nBootstrap coordinationAddr=<coordination_addr> witnessAddr=<witness_addr> \n\n\n"
  if(args.length < 2) println(usage)



  val spvFile = "bootstrapWallet" 

  //init the wallet
  BriefLogFormatter.init()//initVerbose()

  println("loading blockchain...")
  val params = MainNetParams.get()
  val kit = new WalletAppKit(params, new java.io.File("."), spvFile)

  kit.setDiscovery(null)
  kit.setAutoSave(false)
  kit.setBlockingStartup(true)

  kit.startAsync()

  kit.awaitRunning()
  println("done!")

  if(args.contains("getRxAddr")){
    println("\n\n\nrx addr: " + kit.wallet().currentReceiveAddress())

    throw new Exception("break")
  }

  val coordinationAddr0 = args.find(a => a.startsWith("coordinationAddr="))
  val witnessAddr0 = args.find(a => a.startsWith("witnessAddr="))
  val coordinationAddr = coordinationAddr0.get.split("=")(1)
  val witnessAddr = witnessAddr0.get.split("=")(1)

  require(coordinationAddr0.isDefined && witnessAddr0.isDefined, usage)

  val transactionsOnCoordination = kit.wallet().getTransactions(false).iterator().asScala.toArray
  if (transactionsOnCoordination.length > 0) {
    println("it appears the coordination addr has already been used.")
    throw new Exception("cant build election on used coordination addr.")
  }


  val changeAddr = kit.wallet().currentChangeAddress().toBase58

  println("bootstrapping election")
  println(" bootstrap address=" + kit.wallet().currentReceiveAddress())
  println(" coordination address=" + coordinationAddr)
  println(" witness address=" + witnessAddr)
  println(" bootstrapBalance=" + kit.wallet().getBalance)
  println("sending bootstrap transaction...")

  kit.wallet().getWalletTransactions.asScala.foreach(a => println(a))

  //await connections
  println(kit.wallet())
  while (kit.peerGroup().getConnectedPeers.size() < 10) {
    Thread.sleep(1000)
  }

  val subStringsWit = witnessAddr.sliding(5).toSet
  val subStringsCoo = coordinationAddr.sliding(5).toSet

  val witnessAddrPrefix = subStringsWit.filter(s => !subStringsCoo.contains(s) && !changeAddr.contains(s)).head
  val coordinationAddrPrefix = subStringsCoo.filter(s => !subStringsWit.contains(s) && !changeAddr.contains(s)).head

  require(!witnessAddr.contains(coordinationAddrPrefix) && !coordinationAddr.contains(witnessAddrPrefix), "distinct substring problem")

  val initCoordString = "i:" + coordinationAddrPrefix
  val witnessAddrString = ",w:" + witnessAddrPrefix
  val hashString = ",h:1234567"

  val op_returnString = initCoordString + witnessAddrString + hashString

  require(op_returnString.length < 39, "op_return too long")

  //the coordinator
  val reqCoo = Wallet.SendRequest.to(Address.fromBase58(kit.wallet().getNetworkParameters, coordinationAddr), Transaction.MIN_NONDUST_OUTPUT)
  //the witness
  reqCoo.tx.addOutput(Transaction.MIN_NONDUST_OUTPUT, Address.fromBase58(kit.wallet().getNetworkParameters, witnessAddr))
  //the op_return
  reqCoo.tx.addOutput(Transaction.MIN_NONDUST_OUTPUT, new ScriptBuilder().op(ScriptOpCodes.OP_RETURN).data(op_returnString.getBytes()).build())

  reqCoo.feePerKb = Coin.parseCoin("0.0007")

  //ch-ch-ch-chaaannggeee
  reqCoo.changeAddress = Address.fromBase58(kit.wallet().getNetworkParameters, changeAddr)

  //send
  kit.wallet().sendCoins(reqCoo)

  //zzz what was your original face before you were born?
  Thread.sleep(10000)
}
