package com.weelect.witness

import com.weelect.administer.Bootstrap
import Bootstrap._
import com.weelect.audit.ElectionState
import org.bitcoinj.core.{Address, Coin, Wallet, Transaction}
import org.bitcoinj.kits.WalletAppKit
import org.bitcoinj.params.MainNetParams
import org.bitcoinj.script.{ScriptOpCodes, ScriptBuilder}
import org.bitcoinj.utils.BriefLogFormatter
import org.bitcoinj.wallet.WalletTransaction

import scala.io.Source
import scala.collection.JavaConverters._
/**
 * Created by t on 1/28/16.
 */
object Witness extends App {

  BriefLogFormatter.init()//.initVerbose()
  val params = MainNetParams.get()
  println("loading blockchain...")
  val kit = new WalletAppKit(params, new java.io.File("."), "witnessWallet")
  kit.setDiscovery(null)
  kit.setAutoSave(false)
  kit.startAsync()
  kit.awaitRunning()

  println("done!")

  println(kit.wallet())
  if(args.contains("getRxAddr")){
    println("rx addr: " + kit.wallet().currentReceiveAddress())
    throw new Exception("break")
  }

  val coordinationAddr = args.find(_.startsWith("coordinationAddr=")).get.split("=")(1)
  val witnessAddr = args.find(_.startsWith("witnessAddr=")).get.split("=")(1)

  println("Registering voters from witness.csv...")

  val es = new ElectionState(coordinationAddr, witnessAddr, "1234567")

  require(es.isValid, "Invalid election configuration")
  require(kit.wallet().getIssuedReceiveAddresses.asScala.exists(_.toBase58 == es.witnessAddr), "I am not the witness for this election.")

  println("verified state.")

  val alreadyReg = es.voters.map(_.toBase58)
  println("currently registered voters:\n  " + alreadyReg.mkString("\n  "))
  val witFile = args.find(_.startsWith("registerAddrs=")).getOrElse("registerAddrs=witness.csv").split("=")(1)
  val allWitnessAddrs = Source.fromFile(witFile).getLines().toArray.filter(v=> ! alreadyReg.contains(v))

  allWitnessAddrs.sliding(100).foreach { witnessAddrs =>
    val req = Wallet.SendRequest.to(Address.fromBase58(kit.wallet().getNetworkParameters, es.coordinationAddr), Transaction.MIN_NONDUST_OUTPUT)
    req.tx.addOutput(Transaction.MIN_NONDUST_OUTPUT, new ScriptBuilder().op(ScriptOpCodes.OP_RETURN).data("witness".getBytes()).build());
    req.feePerKb = Coin.parseCoin("0.0005")
    req.changeAddress = Address.fromBase58(kit.wallet().getNetworkParameters,es.witnessAddr)
    //add the witnessed addrs
    witnessAddrs.foreach {
      addr =>
        println("registering " + addr)
        req.tx.addOutput(Transaction.MIN_NONDUST_OUTPUT.multiply(5).add(req.feePerKb), Address.fromBase58(kit.wallet().getNetworkParameters, addr))
    }

    kit.wallet().sendCoins(req)
  }


  println("done.")
  while(true){Thread.sleep(1000)}

}
