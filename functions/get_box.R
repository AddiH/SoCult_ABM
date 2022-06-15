get.box <- function(A_choice, B_choice){
  if        (A_choice == "LA1" & B_choice == "LB1") {
    box = 1
  } else if (A_choice == "LA2" & B_choice == "LB1") {
    box = 2 
  } else if (A_choice == "LA1" & B_choice == "LB2") {
    box = 3
  } else if (A_choice == "LA2" & B_choice == "LB2") {
    box = 4
  }
  return(box)
}