
import scala.reflect.ClassTag

/**
 * 环形缓冲区实现
 * @param capacity 缓冲区容量
 * @tparam T 缓冲区元素类型
 */
class RingBuffer[T: ClassTag](capacity: Int) {
  private val buffer = new Array[T](capacity)
  private var head = 0
  private var tail = 0
  private var count = 0

  /**
   * 向缓冲区中添加元素
   * @param item 待添加元素
   * @return 添加是否成功
   */
  def push(item: T): Boolean = {
    // if (isFull) return false
    
    buffer(tail) = item
    tail = (tail + 1) % capacity
    // println(s"push: ${item.toString}")
    count = if(count < capacity) count + 1 else count
    true
  }

  /**
   * 从缓冲区中取出元素
   * @return 取出的元素，如果缓冲区为空则返回None
   */
  def pop(): Option[T] = {
    // if (isEmpty) return None
    
    val item = buffer(head)
    head = (head + 1) % capacity
    // count -= 1
    Some(item)
  }

  /**
   * 获取缓冲区中的元素数量
   * @return 元素数量
   */
  def size: Int = count

  /**
   * 判断缓冲区是否为空
   * @return 如果缓冲区为空则返回true，否则返回false
   */
  def isEmpty: Boolean = count == 0

  /**
   * 判断缓冲区是否已满
   * @return 如果缓冲区已满则返回true，否则返回false
   */
  def isFull: Boolean = count == capacity

  /**
   * 清空缓冲区
   */
  def clear(): Unit = {
    head = 0
    tail = 0
    count = 0
  }

  /**
   * 获取指定位置的元素（不移动指针）
   * @param index 相对于当前head的偏移
   * @return 元素值
   */
  def get(index: Int): Option[T] = {
    if (index < 0 || index >= count) return None
    val pos = (head + index) % capacity
    Some(buffer(pos))
  }

  /**
   * 将缓冲区中的所有元素转换为数组
   * @return 包含所有元素的数组
   */
  def toArray: Array[T] = {
    val result = new Array[T](count).asInstanceOf[Array[T]]
    tail = (tail - count + capacity) % capacity
    for (i <- 0 until count) {
      result(i) = buffer((tail + i) % capacity)
    }
    result
  }
} 