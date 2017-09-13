def getLargest[T <% Ordered[T]](data: List[T]):(T,List[T]) = data match {
	case (Nil) => (null.asInstanceOf[T],Nil)
	case (x::Nil) =>(x,Nil)
	case (x::xs) => {
		val (x1,xs1) = getLargest(xs)
		if(x >= x1){
			(x,x1::xs1)
		}else{
			(x1,x::xs1)
		}
	}
}

val initDataList:List[Integer] = List(1,3,5,2,6)


def bubbleSort[T <% Ordered[T]](data:List[T]):List[T] = data match {
	case Nil => Nil
	case _ =>
		val (big,res) = getLargest(data)
		bubbleSort(res):+big
}

val rr = bubbleSort(initDataList)
println(rr)

println("="*20)
println("select sort")

def selectSort[T <% Ordered[T]](data:List[T]):List[T] = data match {
	case Nil => Nil
	case _ =>
		val (big,res) = getLargest(data)
		big::selectSort(res)
}

val resultSelectSort = selectSort(initDataList)
println(resultSelectSort)

println("="*20)
println("insert sort")

def insertElement[T <% Ordered[T]](elem:T ,sortedList:List[T]):List[T] = sortedList match {
	case Nil =>  List(elem)
	case x::xs =>
		if( elem < x){
			elem :: sortedList
		}else{
			x::insertElement(elem,xs)
		}
}

def insertSort[T <% Ordered[T]](data:List[T]):List[T] = data match {
	case Nil => Nil
	case x:: res =>
		val tmp = insertSort(res)
		insertElement(x,tmp)
}

val resultInsertSort = insertSort(initDataList)

println(resultInsertSort)

println("="*20)
println("merge sort")

def split[T <% Ordered[T]](org:List[T]) :(List[T],List[T]) = org match {
	case Nil => (List.empty[T],List.empty[T])
	case x::Nil => (List(x),Nil)
	case x::y::res =>
		val (res1,res2) = split(res)
		(x::res1,y::res2)
}

def merge[T <% Ordered[T]](x:List[T],y:List[T]):List[T] = (x,y) match {
	case (x,Nil) => x
	case (Nil,y) => y
	case(x1::xs1,y1::ys1) =>
		if(x1 > y1){
			y1::merge(x,ys1)
		}else{
			x1::merge(xs1,y)
		}
}

def mergeSort[T <% Ordered[T]](ls:List[T]):List[T] = {
	if(ls == Nil || ls.tail == Nil)
		return ls
	val (f,s) = split(ls)
	val r1  = mergeSort(f)
	val r2 = mergeSort(s)
	merge(r1,r2)
}

val mergeSortResutl = mergeSort(initDataList)
println(mergeSortResutl)

println("="*20)
println("quick sort in scala")

def quickSort[T <% Ordered[T]](data:List[T]):List[T] = data match {
	case Nil => Nil
	case x::res =>
		val higher = quickSort(data.filter( _ > x))
		val lower = quickSort(data.filter(_ < x))
		(lower:+ x):::higher
}

println(quickSort(initDataList))

