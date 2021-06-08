method Abs(x : int) returns (y : int)
    ensures y >= 0
    ensures x >= 0 ==> x == y
    ensures x <= 0 ==> -x == y
{
    if x < 0 
    { return -x;}
    else 
    {return x;}
}

predicate sorted (a : seq<int>) 
{
    forall j, k :: 0 <= j <= k < |a| ==> a[j] <= a[k]
}

predicate sorted_slice(a : array<int>, l : int, h : int)
requires 0 <= l <= h <= a.Length
reads a
{
    forall j, k :: l <= j <= k < h ==> a[j] <= a[k]
}

predicate sorted_array(a : array<int>)
reads a
{
    forall j, k :: 0 <= j <= k < a.Length ==> a[j] <= a[k]
}

method Merge (a : array<int>, b : array<int>) returns (r : array<int>)
    requires sorted_array(a)
    requires sorted_array(b)
    ensures  sorted_array(r)
{
    var ia := 0;
    var ib := 0;
    var ri := 0;
    r := new int[a.Length + b.Length];
    while ri < r.Length
        decreases r.Length - ri
        invariant ri == ia + ib
        // r[:ri] is sorted
        invariant forall j, k :: (0 <= j <= k < ri) && (0 <= j <= k < r.Length) ==> r[j] <= r[k]
        // a[ai:] > r[:ri]
        invariant forall ja, jr :: ia <= ja < a.Length && 0 <= jr < ri < r.Length ==> r[jr] <= a[ja]
        // b[bi:] > r[:ri]
        invariant forall jb, jr :: ib <= jb < b.Length && 0 <= jr < ri < r.Length ==> r[jr] <= b[jb]
    {
        if ia >= a.Length {
            r[ri] := b[ib];
            ib := ib + 1;
            ri := ri + 1;
        } else if ib >= b.Length {
            r[ri] := a[ia];
            ia := ia + 1;
            ri := ri + 1;
        } else {
            if a[ia] < b[ib]
            {
                r[ri] := a[ia];
                ia := ia + 1;
                ri := ri + 1;
            } else {
                r[ri] := b[ib];
                ib := ib + 1;
                ri := ri + 1;
            }
        }
    }
}

method GetMerged (arr : array<int>, l : int, m : int, r : int) returns (res : array<int>)
    requires 0 <= l < m < r <= arr.Length
    requires sorted_slice(arr, l, m);
    requires sorted_slice(arr, m, r);
    ensures sorted_array(res)
    ensures res.Length == (r - l)
    ensures forall i :: (l <= i < r) ==> exists j :: (0 <= j < res.Length) && arr[i] == res[j]
{
    var ia := l;
    var ib := m;
    res := new int[r - l];
    var ri := 0;
    while ri < res.Length
        decreases res.Length - ri
        invariant ri == (ia - l) + (ib - m)
        
        //Ensure that the ia/ib is within the sorted slice at all times
        invariant l <= ia <= m
        invariant m <= ib <= r

        // r[:ri] is sorted
        invariant forall j, k :: (0 <= j <= k < ri) && (0 <= j <= k < res.Length) ==> res[j] <= res[k]
        invariant forall ja, jr :: (ia <= ja < m) && (0 <= jr < ri < res.Length) ==> res[jr] <= arr[ja]
        invariant forall jb, jr :: (ib <= jb < r) && (0 <= jr < ri < res.Length) ==> res[jr] <= arr[jb]

        // r[:ri] contains all elements from arr currently processed
        invariant forall i :: l <= i < ia ==> exists j :: (0 <= j < ri) && arr[i] == res[j]
    {
        assert forall i :: l <= i < ia ==> exists j :: (0 <= j < ri) && arr[i] == res[j];
        if ia >= m {
            res[ri] := arr[ib];
            ib := ib + 1;
            ri := ri + 1;
        } else if ib >= r {
            res[ri] := arr[ia];
            ia := ia + 1;
            ri := ri + 1;
        } else {
            if arr[ia] < arr[ib]
            {
                res[ri] := arr[ia];
                ia := ia + 1;
                ri := ri + 1;
            } else {
                res[ri] := arr[ib];
                ib := ib + 1;
                ri := ri + 1;
            }
        }
    }
    return res;
}

method MergeSlice (arr : array<int>, l : int, m : int, r : int)
    modifies arr
    requires 0 <= l < m < r <= arr.Length
    requires sorted_slice(arr, l, m);
    requires sorted_slice(arr, m, r);
    ensures sorted_slice(arr, l, r)
{
    var merged := GetMerged(arr, l, m, r);

    var i := l;
    var ri := 0;
    assert i + merged.Length < arr.Length;
    while ri < merged.Length
        invariant i == ri + l
        invariant arr[l..i] == merged[..ri]
    {
        arr[i] := merged[ri];
        ri := ri + 1;
        i := i + 1;
    }
}