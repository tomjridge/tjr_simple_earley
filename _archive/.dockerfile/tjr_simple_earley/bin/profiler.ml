let now =
  Core.(fun () ->
      Time_stamp_counter.(now () |> to_int63)
      |>Int63.to_int_exn)

let profiler = Tjr_profile.make_string_profiler ~now

