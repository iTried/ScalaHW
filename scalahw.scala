println("Hello, world!")

def pascal(c: Int, r: Int): Int = {
    if (c == r || c == 0)
        1
    else
        pascal(c - 1, r - 1) + pascal(c, r - 1)
}


def balance(chars: List[Char]): Boolean = {
    def innerBalance(chars: List[Char], stack: Int): Boolean = {
        if (chars.isEmpty)
            stack == 0
        else
            if (chars.head == '(')
                innerBalance(chars.tail, stack+1)
            else if(chars.head == ')')
                innerBalance(chars.tail, stack-1) && stack != 0
            else
                innerBalance(chars.tail, stack)
    }
    innerBalance(chars, 0)
}


println(balance("(just ()an)() example".toList))

def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0 &&  coins.isEmpty) 1
    else if (money < 0 || coins.isEmpty) 0
    else countChange(money - coins.head, coins) + countChange(money, coins.tail)
}

println( countChange(300,List(500,5,50,100,20,200,10)))
