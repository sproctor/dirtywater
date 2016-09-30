
isLoggedIn = false
while not isLoggedIn do
	send("Welcome to Dirty Water, friend. What name do you go by? If you are new here, type \"new\".")
	name = read()
	if name == "new" then
		createCharacter()
	else
		send("Welcome back, ", name, "! Please enter your password.")
		password = read()
		if login(name, password) then
			isLoggedIn = true
		else
			send("What are you trying to pull here, \"", name, "\"!? Maybe I should call you \"liar\" instead. That password wasn't correct. Let's start this over.")
		end
	end
end
