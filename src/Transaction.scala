import collection._

case class AbortException(private val message: String = "", 
                           private val cause: Throwable = None.orNull)
                      extends Exception(message, cause)

final class Transaction() {
    private val writes = mutable.Map[Ref[Any], Any]()
    private val reads = mutable.Map[Ref[Any], Any]()
    var rev = Transaction.copyrev;
    
    def store[T](ref: Ref[T], value : T)
    {
      val castRef = ref.asInstanceOf[Ref[Any]]
      writes(castRef) = value;
    }
    
    def retrieve[T]  (ref: Ref[T], currentRefValue : T): T =
    {
      if ( writes.contains(ref.asInstanceOf[Ref[Any]]))
      {
         //writes.get(ref.asInstanceOf[Ref[Any]])
         writes.getOrElse(ref.asInstanceOf[Ref[Any]], null).asInstanceOf[T]
      }
      else {
        // Conflict ??
        val newrev = Transaction.copyrev
        reads(ref.asInstanceOf[Ref[Any]]) = currentRefValue;

        if (this.rev != newrev && !ValidateReads(newrev))
          Abort
        currentRefValue
      }        
    }
    
    def CommitTx =
    {
      if (writes.size == 0)
      {
        true
      }
      else {
        var newrev = 0;
        while(newrev % 2 == 0) 
        {
         newrev = Transaction.getrevlock;
         if (newrev == 0) // could not get lock
         {
           Thread sleep 25
         }
        }
        

        
        if (newrev != (rev + 1) && !ValidateReads(newrev) ) // no Transaction committed
        {
           Transaction.releaserev;
           Abort;
        }
        rev = newrev;
        doWrites
        Transaction.releaserev;
        true
      }
    }
    
    def doWrites
    {
      var i = 0;
      for (ref <- writes)
      {
        i = i + 1;
        ref._1.set(ref._2,this);
      }
    }
    
    private def ValidateReads(newrev : Int) = 
    {
      var noConflict = true;
      for (ref <- reads)
      {
        if (!ref._1.compare(ref._2))
        {
          noConflict = false
        }
      }
      this.rev = newrev
      noConflict
    }
    private def Abort = {throw new AbortException()}
}


object Transaction {
  private var rev_ = 0
  private val revLock = new AnyRef
  
  private def getrevlock = revLock.synchronized {
    var returnMe = 0;
    if (rev_ % 2 == 0)
    {
      rev_ += 1
      returnMe = rev_
    }
    returnMe;
    
  }
  
  private def releaserev = revLock.synchronized {
    rev_ += 1
  }
  
  private def copyrev = {
    while (rev_ % 2 != 0)
    {
     Thread sleep 1
    }
    rev_
  }
  
  def printrev()
  {
    println("rev : " + rev_);
  }
  
  def resetrev =
  {
    val oldrev = rev_
    rev_ = 0
    oldrev
  }
  
   def atomic[R](f : (Transaction) => R): R =
    {
      var done = false;
      val result = 
      try
      {
        val Tx = new Transaction();
        val ret = f(Tx);
        done = Tx.CommitTx
        ret
      }
      catch
      {
        case c: AbortException =>
          {
          atomic(f)
          }
      }
      
      result
    }
}

