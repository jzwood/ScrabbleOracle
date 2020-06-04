import json

square = {
    "tile": {
        "letter": 'a',
        "value": 3
    },
    "bonus": "triple-word-bonus"
}


board = [
    [square] * 3,
    [square] * 3,
    [square] * 3,
]


print(json.dumps(board))

api_input = "[[{\"tile\": {\"letter\": \"a\", \"value\": 3}, \"bonus\": \"triple-word-bonus\"}, {\"tile\": {\"letter\": \"a\", \"value\": 3}, \"bonus\": \"triple-word-bonus\"}, {\"tile\": {\"letter\": \"a\", \"value\": 3}, \"bonus\": \"triple-word-bonus\"}], [{\"tile\": {\"letter\": \"a\", \"value\": 3}, \"bonus\": \"triple-word-bonus\"}, {\"tile\": {\"letter\": \"a\", \"value\": 3}, \"bonus\": \"triple-word-bonus\"}, {\"tile\": {\"letter\": \"a\", \"value\": 3}, \"bonus\": \"triple-word-bonus\"}], [{\"tile\": {\"letter\": \"a\", \"value\": 3}, \"bonus\": \"triple-word-bonus\"}, {\"tile\": {\"letter\": \"a\", \"value\": 3}, \"bonus\": \"triple-word-bonus\"}, {\"tile\": {\"letter\": \"a\", \"value\": 3}, \"bonus\": \"triple-word-bonus\"}]]"


entry = {
    "word": "cat",
    "score": 14,
    "coordinates": [[4, 5], [4, 6], [4, 7]]
}
output = [entry] * 12

api_output = "[{\"word\": \"cat\", \"score\": 14, \"coordinates\": [[4, 5], [4, 6], [4, 7]]}, {\"word\": \"cat\", \"score\": 14, \"coordinates\": [[4, 5], [4, 6], [4, 7]]}, {\"word\": \"cat\", \"score\": 14, \"coordinates\": [[4, 5], [4, 6], [4, 7]]}, {\"word\": \"cat\", \"score\": 14, \"coordinates\": [[4, 5], [4, 6], [4, 7]]}, {\"word\": \"cat\", \"score\": 14, \"coordinates\": [[4, 5], [4, 6], [4, 7]]}, {\"word\": \"cat\", \"score\": 14, \"coordinates\": [[4, 5], [4, 6], [4, 7]]}, {\"word\": \"cat\", \"score\": 14, \"coordinates\": [[4, 5], [4, 6], [4, 7]]}, {\"word\": \"cat\", \"score\": 14, \"coordinates\": [[4, 5], [4, 6], [4, 7]]}, {\"word\": \"cat\", \"score\": 14, \"coordinates\": [[4, 5], [4, 6], [4, 7]]}, {\"word\": \"cat\", \"score\": 14, \"coordinates\": [[4, 5], [4, 6], [4, 7]]}, {\"word\": \"cat\", \"score\": 14, \"coordinates\": [[4, 5], [4, 6], [4, 7]]}, {\"word\": \"cat\", \"score\": 14, \"coordinates\": [[4, 5], [4, 6], [4, 7]]}]"
