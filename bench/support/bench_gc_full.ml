type snapshot = Gc.stat

type delta = {
  minor_words : float;
  major_words : float;
  promoted_words : float;
  minor_collections : int;
  major_collections : int;
  forced_major_collections : int;
  compactions : int;
  heap_words_after : int;
  top_heap_words_after : int;
  live_words_after : int;
  free_words_after : int;
  stack_size_after : int;
}

let stabilize () = Gc.full_major ()

let take () = Gc.stat ()

let diff ~(before : snapshot) ~(after : snapshot) =
  {
    minor_words = after.minor_words -. before.minor_words;
    major_words = after.major_words -. before.major_words;
    promoted_words = after.promoted_words -. before.promoted_words;
    minor_collections = after.minor_collections - before.minor_collections;
    major_collections = after.major_collections - before.major_collections;
    forced_major_collections =
      after.forced_major_collections - before.forced_major_collections;
    compactions = after.compactions - before.compactions;
    heap_words_after = after.heap_words;
    top_heap_words_after = after.top_heap_words;
    live_words_after = after.live_words;
    free_words_after = after.free_words;
    stack_size_after = after.stack_size;
  }
