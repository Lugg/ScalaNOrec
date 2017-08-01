

class Ref[T](private var value: T) {
  def get[T] (Tx : Transaction) =
  {
    Tx.retrieve(this, this.value)
  }
  
  def set ( value: T , Tx : Transaction) {
    if (Tx.rev % 2 == 1) // Transaction is in Commit 
    {
      this.value = value;
    }
    else
    {
      Tx.store(this, value);
    }
  }
  
  def compare(value : T) = {
    (this.value == value);
  }
}