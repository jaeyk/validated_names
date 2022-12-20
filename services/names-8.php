<?php
  const KEYS = ['name8'];

  $file = 'names.csv';
  $csv = file_get_contents($file);
  $lines = explode("\n", $csv);
  $num = count($lines);

  $randomLineNumber = rand(0, $num - 1); // get random line number
  $line = $lines[$randomLineNumber];     // get random line
  $array = array_combine(KEYS, str_getcsv($line)); // map values to keys

  header('content-type: application/json');
  echo json_encode($array);
?>