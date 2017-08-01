object BankTransfer {
  type Account = Ref[Int];
  
def transfer( amount: Int, from : Account, to : Account)(Tx : Transaction) =
{
  var fromVal = from.get(Tx);
  var toVal = to.get(Tx);
  from.set(fromVal - amount, Tx);
  to.set(toVal + amount, Tx);
  
}

def readAccountValue(from : Account)(Tx: Transaction) =
{
   from.get(Tx);
   Thread sleep 100
   from.get(Tx);
}
  
  def main(args: Array[String]): Unit = {
      val acc1 = new Account(0);
      val acc2 = new Account(0);
      
      /*
      Transaction.atomic(transfer(50,acc1,acc2))
      println("Account 2: " + Transaction.atomic(readAccountValue(acc2)))
      */
      
      val partsWriter = 9;
      val partsReader = 1;
      
      for (numberOfThreads <- Array(100, 1000, 10000, 100000) )
        for (n <- 1 to 5)
          println(n + ", " + numberOfThreads + ", " + TestRunner.doTests(numberOfThreads, readAccountValue(acc1), transfer(50, acc1,acc2), partsWriter, partsReader))
      
       
  }
}