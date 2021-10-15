// Let's start with defining some types.

type token_supply = { current_stock : nat ; token_price : tez }
type token_shop_storage = (nat, token_supply) map
type return = operation list * token_shop_storage

let owner_address : address =
  ("tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV" : address)



let main (token_kind_index, token_shop_storage : nat * token_shop_storage) : return =

  // Now, we want to retrieve the record of the token index that was passed in the main function; we do this with the map.find_opt. 
  // We use pattern matching to account for the case that there is no record with this index stored in our map.
  let token_kind : token_supply =
    match Map.find_opt (token_kind_index) token_shop_storage with
    | Some k -> k
    | None -> (failwith "Unknown kind of token" : token_supply)
  in
  
  //We need to check if the amount the user sends with their transaction is equal to the token price. 
  //If not, the transaction fails with an error message.
  let () = if Tezos.amount <> token_kind.token_price then
    failwith "Sorry, the token you are trying to purchase has a different price"
  in
  
  //We also need to check if there are still tokens left. If not, then we also fail the transaction with an error message.
  let () = if token_kind.current_stock = 0n then
    failwith "Sorry, the token you are trying to purchase is out of stock"
  in
  
  //Now, we want to decrease the current_stock field in the record of the token the user wants to purchase. We use Map.update and specify the index of the token and pass the new current_stock value, decreasing it by one. We use abs to get a nat from our subtraction. 
  //And we need to specify the map we want to update, our token_shop_storage of course.

  let token_shop_storage = Map.update
    token_kind_index
    (Some { token_kind with current_stock = abs (token_kind.current_stock - 1n) })
    token_shop_storage
  in
  
  // Send the tokens from the purchase to the owner’s address that we specified at the beginning of the contract.
  // We need to create the receiver address and wrap it as a contract unit that represents an account. This will allow us to send the address the tokens. But first, we need to check if the account exists. If it doesn’t, we fail the transaction with an error message.
  let receiver : unit contract =
    match (Tezos.get_contract_opt owner_address : unit contract option) with
    | Some (contract) -> contract
    | None -> (failwith ("Not a contract") : (unit contract))
  in
  
  // We will create a transaction to an account and not a contract, so we will use unit as the first parameter and then pass the amount and the receiver address. We need to create a list of operations that we will return at the end of the contract, which, in this case, will be just one, and then return the list as well as our updated token_shop_storage storage at the end of the contract.
  let payout_operation : operation = 
    Tezos.transaction unit amount receiver 
  in
  let operations : operation list = 
    [payout_operation] 
  in
  ((operations: operation list), token_shop_storage)

  


