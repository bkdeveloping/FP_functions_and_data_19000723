
object Bank {

  class Account(id:String,b:Double,n:Int)
  {

    val nic:String=id
    val accnumber:Int=n
    var balance: Double = b

    override def toString: String ="["+nic +":"+ balance +":"+accnumber+"]"

    //withraw
    def withdraw(a:Double): Unit = {this.balance=this.balance-a}
    //deposit
    def deposit(a:Double): Unit = {this.balance=this.balance+a}
    //Q3
    //transfer
    def transfer(a:Account,b:Double): Unit ={this.withdraw(b)
      a.deposit(b)
    }
  }
  //Q 4

   //Q 4.1
  val overdraft: List[Account] => List[Account] = (b:List[Account])=> b.filter(x=>x.balance<0)
  //sum of all the balance accounts //Question 4.2
  val sum= ( b: List[Account] ) => b.map( x => (x,x.balance) ).reduce( (a , c) => ( c._1 , a._2 + c._2) )

//Q 4.3
  val interest = (b:List[Account])=>b.map((x) => (x.nic,x.accnumber,if(x.balance>0)  (x.balance+(x.balance*0.05)) else (x.balance+(x.balance*0.1)) ))

  def main(args: Array[String]): Unit = {

    val a1 = new Account("78999765v", 10000, 1)
    val a2 = new Account("78798787v", 8900, 2)
    val a3 = new Account("987655989v", -20, 3)
    val a4 = new Account("875789000v", -100, 4)
    val bank: List[Account] = List(a1,a2,a3,a4)


    println("negative balance :" +overdraft(bank))
    println(" ")

    println("sum: " + sum( bank )._2 )
    println(" ")

    a2.transfer(a3,2300)
    println("After transfer RS.2300 to account b from a ")
    println(bank(1))
    println(bank(2))
    println("")

    println("After Applying Interest")
    println(interest(bank))



  }
}
