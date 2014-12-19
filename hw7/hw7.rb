# University of Washington, Programming Languages, Homework 7, hw7.rb

# A little language for 2D geometry objects
#
# each subclass of GeometryExpression, including subclasses of GeometryValue,
#  needs to respond to messages preprocess_prog and eval_prog
#
# each subclass of GeometryValue additionally needs:
#   * shift
#   * intersect, which uses the double-dispatch pattern
#   * intersectPoint, intersectLine, and intersectVerticalLine for
#       for being called by intersect of appropriate clases and doing
#       the correct intersection calculuation
#   * (We would need intersectNoPoints and intersectLineSegment, but these
#      are provided by GeometryValue and should not be overridden.)
#   *  intersectWithSegmentAsLineResult, which is used by
#      intersectLineSegment as described in the assignment
#
# you can define other helper methods, but will not find much need to

# Note: geometry objects should be immutable: assign to fields only during
#       object construction

# Note: For eval_prog, represent environments as arrays of 2-element arrays
# as described in the assignment

class GeometryExpression
  Epsilon = 0.00001
end


class GeometryValue
  E = GeometryExpression::Epsilon

  def eval_prog env;   self end
  def preprocess_prog; self end
  def intersectNoPoints np; np end

  def intersectLineSegment seg
    line_result = self.intersect(two_points_to_line(seg.x1,seg.y1,seg.x2,seg.y2))
    line_result.intersectWithSegmentAsLineResult seg
  end

  # intersect will vary it's call based on the class name, avoids code
  # duplication
  #
  def intersect other
    other.send("intersect#{self.class}", self)
  end

  private

  def real_close(r1,r2)
    (r1 - r2).abs < E
  end

  def real_close_point(x1,y1,x2,y2)
    real_close(x1,x2) && real_close(y1,y2)
  end

  # two_points_to_line could return a Line or a VerticalLine
  #
  def two_points_to_line(x1,y1,x2,y2)
    if real_close(x1,x2)
      VerticalLine.new x1
    else
      m = (y2 - y1).to_f / (x2 - x1)
      b = y1 - m * x1
      Line.new(m,b)
    end
  end

  # checks if value is between two others
  #
  def inbetween(v, e1, e2)
    (e1 - E <= v && v <= e2 + E) || (e2 - E <= v && v <= e1 + E)
  end

end


class NoPoints < GeometryValue
  def shift(dx,dy);       self end
  def intersectPoint p;   self end
  def intersectLine line; self end
  def intersectVerticalLine vline;          self end
  def intersectWithSegmentAsLineResult seg; self end
end


class Point < GeometryValue
  attr_reader :x, :y

  def initialize(x,y)
    @x = x
    @y = y
  end

  def shift(dx,dy)
    Point.new(x+dx, y+dy)
  end

  def intersectPoint p
    real_close_point(x,y,p.x,p.y) ? p : NoPoints.new
  end

  def intersectLine line
    real_close(y, line.m * x + line.b) ? self : NoPoints.new
  end

  def intersectVerticalLine vline
    real_close(x, vline.x) ? self : NoPoints.new
  end

  def intersectWithSegmentAsLineResult seg
    inbetween(x, seg.x1, seg.x2) && inbetween(y, seg.y1, seg.y2) ?
        Point.new(x,y) : NoPoints.new
  end

end


class Line < GeometryValue
  attr_reader :m, :b

  def initialize(m,b)
    @m = m
    @b = b
  end

  def shift(dx, dy)
    Line.new(m, b + dy - (m * dx))
  end

  def intersectPoint p
    p.intersectLine self
  end

  def intersectLine line
    if real_close(m, line.m)
      real_close(b, line.b) ? self : NoPoints.new
    else
      x = (line.b - b) / (m - line.m)
      y = m * x + b
      Point.new(x,y)
    end
  end

  def intersectVerticalLine vline
    Point.new(vline.x, m * vline.x + b)
  end

  def intersectWithSegmentAsLineResult seg
    seg
  end
end


class VerticalLine < GeometryValue
  attr_reader :x

  def initialize x
    @x = x
  end

  def shift(dx, dy)
    VerticalLine.new(x + dx)
  end

  def intersectPoint p
    p.intersectVerticalLine self
  end

  def intersectLine line
    line.intersectVerticalLine self
  end

  def intersectVerticalLine vline
    real_close(x, vline.x) ? self : NoPoints.new
  end

  def intersectWithSegmentAsLineResult seg
    seg
  end
end


class LineSegment < GeometryValue
  attr_reader :x1, :y1, :x2, :y2

  def initialize (x1,y1,x2,y2)
    @x1 = x1
    @y1 = y1
    @x2 = x2
    @y2 = y2
  end

  def shift(dx, dy)
    LineSegment.new(x1+dx,y1+dy,x2+dx,y2+dy)
  end

  def intersectPoint p
    p.intersectLineSegment self
  end

  def intersectLine line
    line.intersectLineSegment self
  end

  def intersectVerticalLine vline
    vline.intersectLineSegment self
  end

  def preprocess_prog
    if real_close_point(x1,y1,x2,y2)
      Point.new(x1,y1)
    elsif (!real_close(x1,x2) && x1 > x2) ||
        (real_close(x1,x2)  && (!real_close(y1,y2) && y1 > y2))
      LineSegment.new(x2,y2,x1,y1)
    else self
    end
  end

  def intersectWithSegmentAsLineResult seg
    real_close(x1, x2) ?
        (y1 < seg.y1 ? seg_intersect(:vert, self, seg) : seg_intersect(:vert, seg, self)) :
        (x1 < seg.x1 ? seg_intersect(:horz, self, seg) : seg_intersect(:horz, seg, self))
  end

  def to_line
    two_points_to_line(x1,y1,x2,y2)
  end

  private

  def seg_intersect(direction, seg1, seg2)
    aXend, aYend, bXstart, bYstart, bXend, bYend =
        [seg1.x2,seg1.y2,seg2.x1,seg2.y1,seg2.x2,seg2.y2]

    a_end, b_start, b_end =
        direction == :vert ? [aYend, bYstart, bYend] : [aXend, bXstart, bXend]

    if real_close(a_end,b_start)
      Point.new(aXend, aYend)
    elsif a_end < b_start
      NoPoints.new
    elsif a_end > b_end
      LineSegment.new(bXstart,bYstart,bXend,bYend)
    else
      LineSegment.new(bXstart,bYstart,aXend,aYend)
    end
  end

end


class Var < GeometryExpression

  def initialize s
    @s = s
  end

  def eval_prog env # remember: do not change this method
    pr = env.assoc @s
    raise "undefined variable" if pr.nil?
    pr[1]
  end

  def preprocess_prog
    self
  end
end


class Let < GeometryExpression

  def initialize(s,e1,e2)
    @s = s
    @e1 = e1
    @e2 = e2
  end

  def eval_prog env
    @e2.eval_prog([[@s, @e1.eval_prog(env)]] + env)
  end

  def preprocess_prog
    Let.new(@s, @e1.preprocess_prog, @e2.preprocess_prog)
  end
end


class Intersect < GeometryExpression

  def initialize(e1,e2)
    @e1 = e1
    @e2 = e2
  end

  def eval_prog env
    @e1.eval_prog(env).intersect(@e2.eval_prog(env))
  end

  def preprocess_prog
    Intersect.new(@e1.preprocess_prog, @e2.preprocess_prog)
  end
end


class Shift < GeometryExpression

  def initialize(dx,dy,e)
    @dx = dx
    @dy = dy
    @e = e
  end

  def eval_prog env
    @e.eval_prog(env).shift(@dx, @dy)
  end

  def preprocess_prog
    Shift.new(@dx, @dy, @e.preprocess_prog)
  end
end