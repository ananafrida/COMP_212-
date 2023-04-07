structure BarnesHut =
struct

  open Mechanics
  structure BB = BoundingBox
  open Plane
  open TestData

  infixr 3 ++
  infixr 4 **
  infixr 4 //
  infixr 3 -->

  datatype bhtree =
      Empty
    | Single of body
    | Cell of (Scalar.scalar * Plane.point) * BB.bbox * bhtree * bhtree * bhtree * bhtree
      (* ((mass, center), box, top-left, top-right, bottom-left, bottom-right) *)

  (* Projects the mass and center from the root node of a bhtree *)
  fun center_of_mass (T : bhtree) : Scalar.scalar * Plane.point =
      case T of
          Empty => (Scalar.zero, Plane.origin)
        | Single (m, p, _) => (m, p)
        | Cell (com, _, _,_,_,_) => com

  (* Note: Doesn't compare velocities as these are unaffected by compute_tree *)
  fun bodyEq ((m1, p1, _) : body, (m2, p2, _) : body) : bool =
      (Scalar.eq (m1, m2)) andalso Plane.pointEqual (p1, p2)

  fun bhtreeEq (t1 : bhtree, t2 : bhtree) : bool =
      case (t1, t2) of
          (Empty, Empty) => true
        | (Single b1, Single b2) => bodyEq (b1, b2)
        | (Cell ((cm1, cp1), bb1, tl1,tr1,bl1,br1), Cell ((cm2, cp2), bb2, tl2,tr2,bl2,br2)) =>
              Scalar.eq (cm1, cm2) andalso
              Plane.pointEqual (cp1, cp2) andalso
              BB.equal (bb1, bb2) andalso 
              bhtreeEq (tl1,tl2) andalso bhtreeEq (tr1,tr2) andalso 
              bhtreeEq (bl1,bl2) andalso bhtreeEq (br1,br2)
        | (_, _) => false

  (* ---------------------------------------------------------------------- *)
  (* TASKS *)
fun helper(m: Scalar.scalar, r: Plane.point): Plane.vec = (Plane.origin --> r) ** m

  (* TASK *)
  (* Compute the barycenter of four points.
     Assumes that all points have nonnegative mass, and 
     that at least one point has strictly positive mass. *)
  fun barycenter ((m1,p1) : (Scalar.scalar * Plane.point),
                  (m2,p2) : (Scalar.scalar * Plane.point),
                  (m3,p3) : (Scalar.scalar * Plane.point),
                  (m4,p4) : (Scalar.scalar * Plane.point)) : Scalar.scalar * Plane.point =
                  let 
                     val sum1 = Scalar.plus(m1, m2)
                     val sum2 = Scalar.plus(sum1, m3)
                     val sum3 = Scalar.plus(sum2, m4)
                  in
                     (sum3, Plane.head((((helper(m1,p1) ++ helper(m2,p2)) ++ helper(m3,p3)) ++ helper(m4,p4)) // sum3))
                  end

  fun test_barycenter() =
      let 
          val (barymass,baryloc) =
              barycenter ((Scalar.one,p00), (Scalar.one,p02), (Scalar.one,p01), (Scalar.plus(Scalar.one,Scalar.one),p44))
      in
          (testb "bmass" (Scalar.eq(barymass, Scalar.fromInt 5)) true;
           testb "bloc" (Plane.pointEqual(baryloc, Plane.fromcoord(Scalar.fromRatio(8,5), Scalar.fromRatio(11,5)))) true)
      end

  (* TASK *)
  (* Compute the four quadrants of the bounding box *)
  fun quarters (bb : BB.bbox) : BB.bbox * BB.bbox * BB.bbox * BB.bbox =
  let
     val (tl,tr,bl,br) = BB.corners(bb)
     val c = BB.center(bb)
  in 
     (BB.from2Points(tl, c), BB.from2Points(tr, c), BB.from2Points(bl,c), BB.from2Points(br,c))
  end

  (* Test for quarters: *)
  fun test_quarters() =
      testb "q1" (let val (tl,tr,bl,br) = quarters(bb4) 
                  in BB.equal(tl,bb0) andalso BB.equal(tr,bb1) andalso
                      BB.equal(bl, bb2) andalso BB.equal(br,bb3)
                  end) true

  (* TASK *)
  (* Computes the Barnes-Hut tree for the bodies in the given sequence.
   * Assumes all the bodies are contained in the given bounding box,
     and that no two bodies have collided (or are so close that dividing the 
     bounding box will not eventually separate them).
     *)
  fun compute_tree (s : body Seq.seq, bb : BB.bbox) : bhtree = (*gotta call smth to recall the sequence of points that belong
  in that quadrant*)
        case (Seq.length(s)) of
      0 => Empty
      |1 => Single(Seq.nth (0,s))
      |_ => let val (tl,tr,bl,br) = quarters(bb)
                val tlbodyseq = Seq.filter (fn (m,p,v)=> BB.contained ((false, false, false, false), p,tl), s)
                val trbodyseq = Seq.filter (fn (m,p,v)=> BB.contained ((true, false, false, false), p,tr), s)
                val blbodyseq = Seq.filter (fn (m,p,v)=> BB.contained ((false, false, true, false), p,bl), s)
                val brbodyseq = Seq.filter (fn (m,p,v)=> BB.contained ((true, false, true, false), p,br), s)
                val (tlBh, trBh, blBh, brBh) = (compute_tree (tlbodyseq, tl), compute_tree (trbodyseq, tr), compute_tree (blbodyseq, bl), compute_tree (brbodyseq, br))
              in Cell(barycenter(center_of_mass(tlBh), center_of_mass(trBh), center_of_mass(blBh), center_of_mass (brBh)), bb, tlBh, trBh, blBh, brBh)
              end


  (* Test for compute_tree: *)
  fun test_compute_tree() =
      let 
          val three_bodies = Seq.cons (body1, Seq.cons (body2, Seq.cons (body3, Seq.empty())))
          val three_bodies_tree = Cell ((Scalar.fromInt 3, p22), bb4,
                                        Cell ((Scalar.fromInt 2, p13), bb0,
                                              Single body3, Empty, Empty, Single body2), 
                                        Empty, 
                                        Empty, 
                                        Single body1)
      in
          testb "c1" (bhtreeEq (compute_tree (three_bodies, bb4), three_bodies_tree)) true
      end

  (* TASK *)
  (* too_far (p1, p2, bb, t) determines if point p1 is "too far" from 
   * a region bb with barycenter p2, given a threshold parameter t,
   * for it to be worth recuring into the region
   *)
  fun too_far (p1 : Plane.point, p2 : Plane.point, bb : BB.bbox, t : Scalar.scalar) : bool =
  Scalar.lte( Scalar.divide( BB.diameter(bb), distance(p1, p2)), t)
      (************No test case for it*************)

  (* TASK *)
  (* Computes the acceleration on b from the tree T using the Barnes-Hut
   * algorithm with threshold t
   *)
  fun bh_acceleration (T : bhtree, t : Scalar.scalar, b : body) : Plane.vec =
  case T of
  Empty => zero
  | Single(a) => Mechanics.accOn(b, a)
  | Cell((m, c), bb, tl, tr, bl, br) => let 
                                           val (s, p, v) = b 
                                        in 
                                           (case too_far(c, p, bb, t) of
                                           true => accOn(b, (m, c, zero))
                                           | false => bh_acceleration(tl, t, b) ++ bh_acceleration(tr, t, b) 
                                           ++ bh_acceleration(bl, t, b) ++ bh_acceleration(br, t, b))
                                        end

  (* TASK
     Given a threshold and a sequence of bodies, compute the acceleration
     on each body using the Barnes-Hut algorithm.
   *)

  fun barnes_hut (threshold : Scalar.scalar, s : body Seq.seq) : Plane.vec Seq.seq = 
  let 
     val s2 = Seq.map(fn (m, p, v) => p, s)
  in 
     Seq.map(fn x => bh_acceleration(compute_tree(s, BB.fromPoints(s2)), threshold, x), s)
  end

  (* Default value of the threshold, theta = 0.5 *)
  val threshold = (Scalar.fromRatio (1,2))

  fun accelerations(bodies : body Seq.seq) : Plane.vec Seq.seq = barnes_hut(threshold, bodies)

end
