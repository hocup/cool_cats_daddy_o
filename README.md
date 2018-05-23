# Cool Cats, Daddy-O

## Introduction

This repository provides the start of an application to display a list of random ["dad jokes"](https://icanhazdadjoke.com), with each joke having a "user" avatar from [placekitten](https://placekitten.com).

Once you [install Elm](https://guide.elm-lang.org/install.html) and clone this repo, you should be able to run `elm-reactor` and browse to `http://localhost:8000/main.elm` to see the application.

The application currently pulls down a random page of jokes. Then for each joke it uses the length of the joke text to determine an avatar.

Your task will be to add new features to the app.
Implement the **2** core features, and pick a bonus feature if time permits.
You shouldn't spend more than **6 hours** on this challenge.
When you are finished, submit a pull request against this repository.

## New Features to Add

#### Core Features

* Calculate and show word count on each joke. (Please write the word count function with built-in String/Regex functions instead of using a library.) Users should be able to sort jokes by word count.
* Add a user-editable list of "profanity" filters that allow users to replace one word with another, e.g.
  * `Replace [ dog ] with [ puppy ]`
  * `Replace [ kitten ] with [ 🐈 ]`

#### Bonus Features

* Switch from getting a random page to allowing a search term. [API Instructions](https://icanhazdadjoke.com/api#search-for-dad-jokes)
* Add "translation" options to show comments in Pig Latin, Lolcat, etc. (You may use a library for this feature).
* Use the joke ID to generate a pseudo-random names for users (e.g. `"21DQnbaaxc" -> "Jane Smith"`)

## Notes

* Some aspects of the features to add are left vague. Please make an implementation decision and document your decision.
* Add tests for any important logic you add (e.g. how you calculate word count).
