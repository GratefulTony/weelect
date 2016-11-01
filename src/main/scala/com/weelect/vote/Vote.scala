package com.weelect.vote

/**
 * Created by t on 2/2/16.
 */

import com.weelect.administer.Bootstrap
import Bootstrap._
import com.weelect.audit.ElectionState
import com.weelect.witness.Witness
import Witness._
import Witness.kit
import Witness.params
import org.bitcoinj.core.{Address, Coin, Transaction, Wallet}
import org.bitcoinj.kits.WalletAppKit
import org.bitcoinj.params.MainNetParams
import org.bitcoinj.script.{ScriptBuilder, ScriptOpCodes}
import org.bitcoinj.utils.BriefLogFormatter

import scala.collection.JavaConverters._


object Vote extends App {
  val psvFile = "voterWallet"

  val coordinationAddr = args.find(_.startsWith("coordinationAddr=")).get.split("=")(1)
  val witnessAddr = args.find(_.startsWith("witnessAddr=")).get.split("=")(1)
  val voteFor = args.find(_.startsWith("voteFor=")).get.split("=")(1)

  BriefLogFormatter.init//Verbose()
  val params = MainNetParams.get()
  val kit = new WalletAppKit(params, new java.io.File("."), psvFile)
  kit.setDiscovery(null)
  kit.setAutoSave(false)
  kit.startAsync()
  kit.awaitRunning()

  val rx = kit.wallet().currentReceiveAddress()

  println(kit.wallet())
  if(args.contains("getRxAddr")){
    println("rx addr: " + rx)
    throw new Exception("break")
  }


  val es = new ElectionState("1DXqsqqLh5Bej6pKrXw9aQxWXoEC5Qrbnw", "14rQHjHEcjNLd2mmq1jBTrJ9yVZ2xS6kax", "1234567")

  require(es.isValid, "Invalid election configuration")

  println("verified state.")

  val alreadyReg = es.voters.map(_.toBase58)

  val myAddrs = kit.wallet().getIssuedReceiveAddresses.asScala.map(_.toBase58)
  val myValidatedAddrs = myAddrs.filter(alreadyReg.contains(_))

  require(myValidatedAddrs.length > 0, "we are not yet registered. Please request registration for " + rx + " and allow time to verify registration (20 min)")
  println(myValidatedAddrs.mkString(", "))


  println(es.votes)
  val req = Wallet.SendRequest.to(Address.fromBase58(kit.wallet().getNetworkParameters, es.coordinationAddr), Transaction.MIN_NONDUST_OUTPUT)
  req.tx.addOutput(Transaction.MIN_NONDUST_OUTPUT, new ScriptBuilder().op(ScriptOpCodes.OP_RETURN).data("voteFor".getBytes()).build());
  req.feePerKb = Coin.parseCoin("0.0005")

  req.changeAddress = Address.fromBase58(params,myValidatedAddrs.head)
  req.tx.addOutput(Transaction.MIN_NONDUST_OUTPUT, Address.fromBase58(kit.wallet().getNetworkParameters, voteFor))

  kit.wallet().sendCoins(req)




  while(true){Thread.sleep(1000)}
}
