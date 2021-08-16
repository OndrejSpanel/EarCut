package net.opengrabeso.earcut4s

import scala.util.control.Breaks._

object EarCut {
  /* ondrejspanel.github.io/ScalaFromJS/: Web*/
  // from https://github.com/mapbox/earcut/blob/master/src/earcut.js


  def earcut(data: Array[Double], holeIndices: Array[Int] = Array.empty, dim: Int = 2): Array[Int] = {
    val hasHoles = holeIndices.nonEmpty
    val outerLen = if (hasHoles) holeIndices(0) * dim else data.length
    var outerNode = linkedList(data, 0, outerLen, dim, true)
    val triangles = JSArray.empty[Int]
    if (outerNode == null || outerNode.next == outerNode.prev) return triangles
    var minX: Double = 0
    var minY: Double = 0
    var maxX: Double = 0
    var maxY: Double = 0
    var x: Double = 0
    var y: Double = 0
    var invSize: Double = 0
    if (hasHoles) outerNode = eliminateHoles(data, holeIndices, outerNode, dim)
    // if the shape is not too simple, we'll use z-order curve hash later; calculate polygon bbox
    if (data.length > 80 * dim) {
      minX = data(0)
      maxX = data(0)
      minY = data(1)
      maxY = data(1)
      for (i <- dim until outerLen by dim) {
        x = data(i)
        y = data(i + 1)
        if (x < minX) minX = x
        if (y < minY) minY = y
        if (x > maxX) maxX = x
        if (y > maxY) maxY = y
      }
      // minX, minY and invSize are later used to transform coords into integers for z-order calculation
      invSize = math.max(maxX - minX, maxY - minY)
      invSize = if (invSize != 0) 1 / invSize else 0
    }
    earcutLinked(outerNode, triangles, dim, minX, minY, invSize)
    triangles
  }
  // create a circular doubly linked list from polygon points in the specified winding order

  def linkedList(data: Array[Double], start: Int, end: Int, dim: Int, clockwise: Boolean) = {
    var i = 0
    var last: Node = null
    if (clockwise == signedArea(data, start, end, dim) > 0) {
      for (i <- start until end by dim) last = insertNode(i, data(i), data(i + 1), last)
    } else {
      for (i <- end - dim to start by -dim) last = insertNode(i, data(i), data(i + 1), last)
    }
    if (last != null && equals(last, last.next)) {
      removeNode(last)
      last = last.next
    }
    last
  }
  // eliminate colinear or duplicate points

  def filterPoints(start: Node, end_par: Node = null): Node = {
    if (!start) return start
    var end = if (end_par != null) end_par else start
    var p = start
    var again: Boolean = false
    breakable {
      do {
        again = false
        if (!p.steiner && (equals(p, p.next) || area(p.prev, p, p.next) == 0)) {
          removeNode(p)
          p = p.prev
          end = p.prev
          if (p == p.next) break()
          again = true
        } else {
          p = p.next
        }
      } while (again || p != end)
    }
    end
  }
  // main ear slicing loop which triangulates a polygon (given as a linked list)

  def earcutLinked(ear_par: Node, triangles: JSArray[Int], dim: Int, minX: Double, minY: Double, invSize: Double, pass: Int = 0): Unit = {
    var ear = ear_par
    if (!ear) return
    // interlink polygon nodes in z-order
    if (!pass && invSize) indexCurve(ear, minX, minY, invSize)
    var stop = ear
    var prev: Node = null
    var next: Node = null
    // iterate through ears, slicing them one by one
    breakable {
      while (ear.prev != ear.next) {
        prev = ear.prev
        next = ear.next
        if (if (invSize) isEarHashed(ear, minX, minY, invSize) else isEar(ear)) {
          // cut off the triangle
          triangles.push(prev.i / dim)
          triangles.push(ear.i / dim)
          triangles.push(next.i / dim)
          removeNode(ear)
          // skipping the next vertex leads to less sliver triangles
          ear = next.next
          stop = next.next
          /* Unsupported: Continue */
        } else {
          ear = next
          // if we looped through the whole remaining polygon and can't find any more ears
          if (ear == stop) {
            // try filtering points and slicing again
            if (!pass) {
              earcutLinked(filterPoints(ear), triangles, dim, minX, minY, invSize, 1)
              // if this didn't work, try curing all small self-intersections locally
            } else if (pass == 1) {
              ear = cureLocalIntersections(filterPoints(ear), triangles, dim)
              earcutLinked(ear, triangles, dim, minX, minY, invSize, 2)
              // as a last resort, try splitting the remaining polygon into two
            } else if (pass == 2) {
              splitEarcut(ear, triangles, dim, minX, minY, invSize)
            }
            break()
          }
        }
      }
    }
  }
  // check whether a polygon node forms a valid ear with adjacent nodes

  def isEar(ear: Node): Boolean = {
    val a = ear.prev
    val b = ear
    val c = ear.next
    if (area(a, b, c) >= 0) return false // reflex, can't be an ear
    // now make sure we don't have other points inside the potential ear
    var p = ear.next.next
    while (p != ear.prev) {
      if (pointInTriangle(a.x, a.y, b.x, b.y, c.x, c.y, p.x, p.y) && area(p.prev, p, p.next) >= 0) return false
      p = p.next
    }
    true
  }

  def isEarHashed(ear: Node, minX: Double, minY: Double, invSize: Double): Boolean = {
    val a = ear.prev
    val b = ear
    val c = ear.next
    if (area(a, b, c) >= 0) return false // reflex, can't be an ear
    // triangle bbox; min & max are calculated like this for speed
    val minTX = if (a.x < b.x) if (a.x < c.x) a.x else c.x else if (b.x < c.x) b.x else c.x
    val minTY = if (a.y < b.y) if (a.y < c.y) a.y else c.y else if (b.y < c.y) b.y else c.y
    val maxTX = if (a.x > b.x) if (a.x > c.x) a.x else c.x else if (b.x > c.x) b.x else c.x
    val maxTY = if (a.y > b.y) if (a.y > c.y) a.y else c.y else if (b.y > c.y) b.y else c.y
    // z-order range for the current triangle bbox;
    val minZ = zOrder(minTX, minTY, minX, minY, invSize)
    val maxZ = zOrder(maxTX, maxTY, minX, minY, invSize)
    var p = ear.prevZ
    var n = ear.nextZ
    // look for points inside the triangle in both directions
    while (p && p.z >= minZ && n && n.z <= maxZ) {
      if (p != ear.prev && p != ear.next && pointInTriangle(a.x, a.y, b.x, b.y, c.x, c.y, p.x, p.y) && area(p.prev, p, p.next) >= 0) return false
      p = p.prevZ
      if (n != ear.prev && n != ear.next && pointInTriangle(a.x, a.y, b.x, b.y, c.x, c.y, n.x, n.y) && area(n.prev, n, n.next) >= 0) return false
      n = n.nextZ
    }
    // look for remaining points in decreasing z-order
    while (p && p.z >= minZ) {
      if (p != ear.prev && p != ear.next && pointInTriangle(a.x, a.y, b.x, b.y, c.x, c.y, p.x, p.y) && area(p.prev, p, p.next) >= 0) return false
      p = p.prevZ
    }
    // look for remaining points in increasing z-order
    while (n && n.z <= maxZ) {
      if (n != ear.prev && n != ear.next && pointInTriangle(a.x, a.y, b.x, b.y, c.x, c.y, n.x, n.y) && area(n.prev, n, n.next) >= 0) return false
      n = n.nextZ
    }
    true
  }
  // go through all polygon nodes and cure small local self-intersections

  def cureLocalIntersections(start_par: Node, triangles: JSArray[Int], dim: Int) = {
    var start = start_par
    var p = start
    do {
      val a = p.prev
      val b = p.next.next
      if (!equals(a, b) && intersects(a, p, p.next, b) && locallyInside(a, b) && locallyInside(b, a)) {
        triangles.push(a.i / dim)
        triangles.push(p.i / dim)
        triangles.push(b.i / dim)
        // remove two nodes involved
        removeNode(p)
        removeNode(p.next)
        p = b
        start = b
      }
      p = p.next
    } while (p != start)
    filterPoints(p)
  }
  // try splitting polygon into two and triangulate them independently

  def splitEarcut(start: Node, triangles: JSArray[Int], dim: Int, minX: Double, minY: Double, invSize: Double): Unit = {
    // look for a valid diagonal that divides the polygon into two
    var a = start
    do {
      var b = a.next.next
      while (b != a.prev) {
        if (a.i != b.i && isValidDiagonal(a, b)) {
          // split the polygon in two by the diagonal
          var c = splitPolygon(a, b)
          // filter colinear points around the cuts
          a = filterPoints(a, a.next)
          c = filterPoints(c, c.next)
          // run earcut on each half
          earcutLinked(a, triangles, dim, minX, minY, invSize)
          earcutLinked(c, triangles, dim, minX, minY, invSize)
          return
        }
        b = b.next
      }
      a = a.next
    } while (a != start)
  }
  // link every hole into the outer loop, producing a single-ring polygon without holes

  def eliminateHoles(data: Array[Double], holeIndices: Array[Int], outerNode_par: Node, dim: Int) = {
    var outerNode = outerNode_par
    val queue = JSArray.empty[Node]
    var i = 0
    var len = 0
    var start: Int = 0
    var end: Int = 0
    var list: Node = null
    for (i <- holeIndices.indices) {
      start = holeIndices(i) * dim
      end = if (i < len - 1) holeIndices(i + 1) * dim else data.length
      list = linkedList(data, start, end, dim, false)
      if (list == list.next) list.steiner = true
      queue.push(getLeftmost(list))
    }
    queue.sort(compareX)
    // process holes from left to right
    for (i <- queue) {
      outerNode = eliminateHole(i, outerNode)
      outerNode = filterPoints(outerNode, outerNode.next)
    }
    outerNode
  }

  def compareX(a: Node, b: Node) = {
    a.x - b.x
  }
  // find a bridge between vertices that connects hole with an outer ring and and link it

  def eliminateHole(hole: Node, outerNode: Node): Node = {
    val bridge = findHoleBridge(hole, outerNode)
    if (!bridge) {
      return outerNode
    }
    val bridgeReverse = splitPolygon(bridge, hole)
    // filter collinear points around the cuts
    val filteredBridge = filterPoints(bridge, bridge.next)
    filterPoints(bridgeReverse, bridgeReverse.next)
    // Check if input node was removed by the filtering
    if (outerNode == bridge) filteredBridge else outerNode
  }
  // David Eberly's algorithm for finding a bridge between hole and outer polygon

  def findHoleBridge(hole: Node, outerNode: Node): Node = {
    var p = outerNode
    val hx = hole.x
    val hy = hole.y
    var qx = -Infinity
    var m: Node = null
    // find a segment intersected by a ray from the hole's leftmost point to the left;
    // segment's endpoint with lesser x will be potential connection point
    do {
      if (hy <= p.y && hy >= p.next.y && p.next.y != p.y) {
        val x = p.x + (hy - p.y) * (p.next.x - p.x) / (p.next.y - p.y)
        if (x <= hx && x > qx) {
          qx = x
          if (x == hx) {
            if (hy == p.y) return p
            if (hy == p.next.y) return p.next
          }
          m = if (p.x < p.next.x) p else p.next
        }
      }
      p = p.next
    } while (p != outerNode)
    if (!m) return null
    if (hx == qx) return m // hole touches outer segment; pick leftmost endpoint
    // look for points inside the triangle of hole point, segment intersection and endpoint;
    // if there are no points found, we have a valid connection;
    // otherwise choose the point of the minimum angle with the ray as connection point
    val stop = m
    val mx = m.x
    val my = m.y
    var tanMin = Infinity
    var tan: Double = 0
    p = m
    do {
      if (hx >= p.x && p.x >= mx && hx != p.x && pointInTriangle(if (hy < my) hx else qx, hy, mx, my, if (hy < my) qx else hx, hy, p.x, p.y)) {
        tan = Math.abs(hy - p.y) / (hx - p.x) // tangential
        if (locallyInside(p, hole) && (tan < tanMin || tan == tanMin && (p.x > m.x || p.x == m.x && sectorContainsSector(m, p)))) {
          m = p
          tanMin = tan
        }
      }
      p = p.next
    } while (p != stop)
    m
  }
  // whether sector in vertex m contains sector in vertex p in the same coordinates

  def sectorContainsSector(m: Node, p: Node) = {
    area(m.prev, m, p.prev) < 0 && area(p.next, m, m.next) < 0
  }
  // interlink polygon nodes in z-order

  def indexCurve(start: Node, minX: Double, minY: Double, invSize: Double) = {
    var p = start
    do {
      if (p.z == 0) p.z = zOrder(p.x, p.y, minX, minY, invSize)
      p.prevZ = p.prev
      p.nextZ = p.next
      p = p.next
    } while (p != start)
    p.prevZ.nextZ = null
    p.prevZ = null
    sortLinked(p)
  }
  // Simon Tatham's linked list merge sort algorithm
  // http://www.chiark.greenend.org.uk/~sgtatham/algorithms/listsort.html

  def sortLinked(list_par: Node) = {
    var list = list_par
    var i = 0
    var p: Node = null
    var q: Node = null
    var e: Node = null
    var tail: Node = null
    var numMerges: Int = 0
    var pSize: Double = 0
    var qSize: Double = 0
    var inSize = 1
    breakable {
      do {
        p = list
        list = null
        tail = null
        numMerges = 0
        while (p) {
          numMerges += 1
          q = p
          pSize = 0
          for (i <- 0 until inSize) {
            pSize += 1
            q = q.nextZ
            if (q != null) break()
          }
          qSize = inSize
          while (pSize > 0 || qSize > 0 && q != null) {
            if (pSize != 0 && (qSize == 0 || !q || p.z <= q.z)) {
              e = p
              p = p.nextZ
              pSize -= 1
            } else {
              e = q
              q = q.nextZ
              qSize -= 1
            }
            if (tail) tail.nextZ = e
            else list = e
            e.prevZ = tail
            tail = e
          }
          p = q
        }
        tail.nextZ = null
        inSize *= 2
      } while (numMerges > 1)
    }
    list
  }
  // z-order of a point given coords and inverse of the longer side of data bbox

  def zOrder(x_par: Double, y_par: Double, minX: Double, minY: Double, invSize: Double): Int = {
    // coords are transformed into non-negative 15-bit integer range
    var x = (32767 * (x_par - minX) * invSize).toInt
    var y = (32767 * (y_par - minY) * invSize).toInt
    x = (x | (x << 8)) & 0x00FF00FF
    x = (x | (x << 4)) & 0x0F0F0F0F
    x = (x | (x << 2)) & 0x33333333
    x = (x | (x << 1)) & 0x55555555
    y = (y | (y << 8)) & 0x00FF00FF
    y = (y | (y << 4)) & 0x0F0F0F0F
    y = (y | (y << 2)) & 0x33333333
    y = (y | (y << 1)) & 0x55555555
    x | (y << 1)
  }
  // find the leftmost node of a polygon ring

  def getLeftmost(start: Node) = {
    var p = start
    var leftmost = start
    do {
      if (p.x < leftmost.x || p.x == leftmost.x && p.y < leftmost.y) leftmost = p
      p = p.next
    } while (p != start)
    leftmost
  }
  // check if a point lies within a convex triangle

  def pointInTriangle(ax: Double, ay: Double, bx: Double, by: Double, cx: Double, cy: Double, px: Double, py: Double) = {
    (cx - px) * (ay - py) - (ax - px) * (cy - py) >= 0 && (ax - px) * (by - py) - (bx - px) * (ay - py) >= 0 && (bx - px) * (cy - py) - (cx - px) * (by - py) >= 0
  }
  // check if a diagonal between two polygon nodes is valid (lies in polygon interior)

  def isValidDiagonal(a: Node, b: Node) = {
    a.next.i != b.i && a.prev.i != b.i && !intersectsPolygon(a, b) && ( // dones't intersect other edges
      locallyInside(a, b) && locallyInside(b, a) && middleInside(a, b) && ( // locally visible
        area(a.prev, a, b.prev) || area(a, b.prev, b)) ||  // does not create opposite-facing sectors
        equals(a, b) && area(a.prev, a, a.next) > 0 && area(b.prev, b, b.next) > 0) // special zero-length case
  }
  // signed area of a triangle

  def area(p: Node, q: Node, r: Node) = {
    (q.y - p.y) * (r.x - q.x) - (q.x - p.x) * (r.y - q.y)
  }
  // check if two points are equal

  def equals(p1: Node, p2: Node) = {
    p1.x == p2.x && p1.y == p2.y
  }
  // check if two segments intersect

  def intersects(p1: Node, q1: Node, p2: Node, q2: Node): Boolean = {
    val o1 = sign(area(p1, q1, p2))
    val o2 = sign(area(p1, q1, q2))
    val o3 = sign(area(p2, q2, p1))
    val o4 = sign(area(p2, q2, q1))
    if (o1 != o2 && o3 != o4) return true // general case
    if (o1 == 0 && onSegment(p1, p2, q1)) return true // p1, q1 and p2 are collinear and p2 lies on p1q1
    if (o2 == 0 && onSegment(p1, q2, q1)) return true // p1, q1 and q2 are collinear and q2 lies on p1q1
    if (o3 == 0 && onSegment(p2, p1, q2)) return true // p2, q2 and p1 are collinear and p1 lies on p2q2
    if (o4 == 0 && onSegment(p2, q1, q2)) return true // p2, q2 and q1 are collinear and q1 lies on p2q2
    false
  }
  // for collinear points p, q, r, check if point q lies on segment pr

  def onSegment(p: Node, q: Node, r: Node) = {
    q.x <= Math.max(p.x, r.x) && q.x >= Math.min(p.x, r.x) && q.y <= Math.max(p.y, r.y) && q.y >= Math.min(p.y, r.y)
  }

  def sign(num: Double) = {
    if (num > 0) 1 else if (num < 0) -1 else 0
  }
  // check if a polygon diagonal intersects any polygon segments

  def intersectsPolygon(a: Node, b: Node): Boolean = {
    var p = a
    do {
      if (p.i != a.i && p.next.i != a.i && p.i != b.i && p.next.i != b.i && intersects(p, p.next, a, b)) return true
      p = p.next
    } while (p != a)
    false
  }
  // check if a polygon diagonal is locally inside the polygon

  def locallyInside(a: Node, b: Node) = {
    if (area(a.prev, a, a.next) < 0) area(a, b, a.next) >= 0 && area(a, a.prev, b) >= 0 else area(a, b, a.prev) < 0 || area(a, a.next, b) < 0
  }
  // check if the middle point of a polygon diagonal is inside the polygon

  def middleInside(a: Node, b: Node) = {
    var p = a
    var inside = false
    val px = (a.x + b.x) / 2
    val py = (a.y + b.y) / 2
    do {
      if (p.y > py != p.next.y > py && p.next.y != p.y && px < (p.next.x - p.x) * (py - p.y) / (p.next.y - p.y) + p.x) inside = !inside
      p = p.next
    } while (p != a)
    inside
  }
  // link two polygon vertices with a bridge; if the vertices belong to the same ring, it splits polygon into two;
  // if one belongs to the outer ring and another to a hole, it merges it into a single ring

  def splitPolygon(a: Node, b: Node) = {
    val a2 = new Node(a.i, a.x, a.y)
    val b2 = new Node(b.i, b.x, b.y)
    val an = a.next
    val bp = b.prev
    a.next = b
    b.prev = a
    a2.next = an
    an.prev = a2
    b2.next = a2
    a2.prev = b2
    bp.next = b2
    b2.prev = bp
    b2
  }
  // create a node and optionally link it with previous one (in a circular doubly linked list)

  def insertNode(i: Int, x: Double, y: Double, last: Node): Node = {
    val p = new Node(i, x, y)
    if (!last) {
      p.prev = p
      p.next = p
    } else {
      p.next = last.next
      p.prev = last
      last.next.prev = p
      last.next = p
    }
    p
  }

  def removeNode(p: Node): Unit = {
    p.next.prev = p.prev
    p.prev.next = p.next
    if (p.prevZ) p.prevZ.nextZ = p.nextZ
    if (p.nextZ) p.nextZ.prevZ = p.prevZ
  }

  class Node(var i: Int, var x: Double, var y: Double) {
    var prev: Node = null
    var next: Node = null
    var z: Int = 0
    var prevZ: Node = null
    var nextZ: Node = null
    var steiner: Boolean = false
  }

  // return a percentage difference between the polygon area and its triangulation area;
  // used to verify correctness of triangulation
  def deviation(data: Array[Double], holeIndices: Array[Int], dim: Int, triangles: Array[Int]) = {
    val hasHoles = holeIndices && holeIndices.length
    val outerLen = if (hasHoles) holeIndices(0) * dim else data.length
    var polygonArea = Math.abs(signedArea(data, 0, outerLen, dim))
    if (hasHoles) {
      val len = holeIndices.length
      for (i <- holeIndices.indices) {
        val start = holeIndices(i) * dim
        val end = if (i < len - 1) holeIndices(i + 1) * dim else data.length
        polygonArea -= Math.abs(signedArea(data, start, end, dim))
      }
    }
    var trianglesArea: Double = 0
    for (i <- 0 until triangles.length by 3) {
      val a = triangles(i) * dim
      val b = triangles(i + 1) * dim
      val c = triangles(i + 2) * dim
      trianglesArea += Math.abs((data(a) - data(c)) * (data(b + 1) - data(a + 1)) - (data(a) - data(b)) * (data(c + 1) - data(a + 1)))
    }
    if (polygonArea == 0 && trianglesArea == 0) 0 else Math.abs((trianglesArea - polygonArea) / polygonArea)
  }

  def signedArea(data: Array[Double], start: Int, end: Int, dim: Int) = {
    var sum: Double = 0
    var i = start
    var j = end - dim
    while (i < end) {
      {
        sum += (data(j) - data(i)) * (data(i + 1) + data(j + 1))
        j = i
      }
      i += dim
    }
    sum
  }

  case class FlattenResult(vertices: Array[Double], holes: Array[Int], dimensions: Int)

  // turn a polygon in a multi-dimensional array form (e.g. as in GeoJSON) into a form Earcut accepts
  def flatten(data: Array[Array[Array[Double]]]) = {
    val dim = data(0)(0).length
    object result {
      val vertices = JSArray[Double]()
      val holes = JSArray[Int]()
      val dimensions = dim
    }
    var holeIndex = 0
    for (i <- data.indices) {
      for (j <- data(i).indices) {
        for (d <- 0 until dim) result.vertices.push(data(i)(j)(d))
      }
      if (i > 0) {
        holeIndex += data(i - 1).length
        result.holes.push(holeIndex)
      }
    }
    FlattenResult(result.vertices, result.holes, result.dimensions)

  }



}
