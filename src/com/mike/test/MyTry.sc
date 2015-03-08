val func:PartialFunction[Any, String] = {case x:Int => "Good"}

func(20)
func("F")