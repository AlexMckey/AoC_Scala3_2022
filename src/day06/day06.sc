val s = "mjqjpqmgbljsphdztnvjfqwrcgsmlb"
//val s = "bvwbjplbgvbhsrlpgdmjqwftvncz"
//val s = "nppdvjthqldpwncqszvftbrmjlhg"
//val s = "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"
//val s = "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"
val ss1 = s.sliding(4).zipWithIndex.toList
ss1.find((s,_) => s.distinct.length == 4)
ss1.find((s,_) => s.forall(c => s.count(_ == c) <= 1)).map(_._2+4)
val ss2 = s.sliding(14).zipWithIndex.toList
ss2.find((s,_) => s.forall(c => s.count(_ == c) <= 1)).map(_._2+14)
