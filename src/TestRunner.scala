

object TestRunner {
  def doTests(numOfThreads: Int, reader : (Transaction) => Any, writer : (Transaction) => Any, partsWriter: Int, partsReader: Int) =
  {
    val threads = Array.fill[Thread](numOfThreads + partsReader + partsWriter + 1){null}
    var writers = 0;
    var threadsCreated = 0;
    while (threadsCreated < numOfThreads)
    {
      for (i <- 1 to partsReader) {
        threadsCreated += 1
        threads(threadsCreated) = new Thread {
          override def run {
            Transaction.atomic(reader); 
          }
        }
        threads(threadsCreated).start()
      }
      
      for (i <- 1 to partsWriter) {
        writers = writers +1;
        threadsCreated += 1
        threads(threadsCreated) = new Thread {
          override def run {
            Transaction.atomic(writer); 
          }
        }
        threads(threadsCreated).start()
      }
    }
    
    for (i <- 1 to threadsCreated)
    {
      threads(i).join();
      //println("fin: " + i)
    }
    (Transaction.resetrev / 2) - writers // returns number of retries
  }
}