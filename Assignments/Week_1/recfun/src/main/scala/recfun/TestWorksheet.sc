def balance(chars: List[Char]): Boolean = {
  def accumulatedBalance(chars: List[Char], acc: Int): Boolean = {
    if (acc < 0) false
    else if ((acc > 0)  && chars.isEmpty) false
    else if ((acc == 0) && chars.isEmpty) true
    else if (chars.head == '(') accumulatedBalance(chars.tail, acc + 1)
    else if (chars.head == ')') accumulatedBalance(chars.tail, acc - 1)
    else accumulatedBalance(chars.tail, acc)
  }

  accumulatedBalance(chars, 0)
}

balance("(if (zero? x) max (/ 1 x))".toList)
balance(":-)".toList)
