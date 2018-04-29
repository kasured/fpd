def concatR(xs: List[Int],ys: List[Int]): List[Int] = {
  xs.foldRight(ys)((x, list) => x :: list)
}

def concatL(xs: List[Int],ys: List[Int]): List[Int] = {
  xs.fold(ys)((x, list) => x :: list)
}

concatR(List(1,2,3), List(3,4,5))