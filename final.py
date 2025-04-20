import pandas as pd

# Read the CSV file into a DataFrame
df = pd.read_csv('taylor_swift_spotify.csv')

# Display the DataFrame to verify column names and data
print(df.head())

# Sort the DataFrame by popularity in descending order
df_sorted = df.sort_values(by='popularity', ascending=True)

# Define a Node class for the Binary Search Tree (BST)
class Node:
    def __init__(self, song_name, album, release_date, acousticness, danceability, energy, instrumentalness, liveness, loudness, speechiness, tempo, valence, popularity):
        # Initialize node attributes
        self.song_name = song_name
        self.album = album
        self.release_date = release_date
        self.acousticness = acousticness
        self.danceability = danceability
        self.energy = energy
        self.instrumentalness = instrumentalness
        self.liveness = liveness
        self.loudness = loudness
        self.speechiness = speechiness
        self.tempo = tempo
        self.valence = valence
        self.popularity = popularity
        # Initialize left and right child nodes
        self.left = None
        self.right = None

# Define a Binary Search Tree (BST) class
class BST:
    def __init__(self):
        self.root = None
        self.results_stack = []  # Stack to store comparison results

    # Insert method to add nodes to the BST
    def insert(self, song_name, album, release_date, acousticness, danceability, energy, instrumentalness, liveness, loudness, speechiness, tempo, valence, popularity):
        self.root = self._insert_recursive(self.root, song_name, album, release_date, acousticness, danceability, energy, instrumentalness, liveness, loudness, speechiness, tempo, valence, popularity)

    # Recursive helper function for insert method
    def _insert_recursive(self, node, song_name, album, release_date, acousticness, danceability, energy, instrumentalness, liveness, loudness, speechiness, tempo, valence, popularity):
        if node is None:
            return Node(song_name, album, release_date, acousticness, danceability, energy, instrumentalness, liveness, loudness, speechiness, tempo, valence, popularity)

        if song_name < node.song_name:
            node.left = self._insert_recursive(node.left, song_name, album, release_date, acousticness, danceability, energy, instrumentalness, liveness, loudness, speechiness, tempo, valence, popularity)
        elif song_name > node.song_name:
            node.right = self._insert_recursive(node.right, song_name, album, release_date, acousticness, danceability, energy, instrumentalness, liveness, loudness, speechiness, tempo, valence, popularity)

        return node

    # Search method to find a node in the BST
    def search(self, node, song_name):
        if node is None:
            return None
        if song_name.strip() == node.song_name.strip():
            return node
        elif song_name < node.song_name:
            return self.search(node.left, song_name)
        else:
            return self.search(node.right, song_name)

    # Compare attributes of two songs based on a specified attribute
    def compare_attributes(self, song_name1, song_name2, attribute):
        node1 = self.search(self.root, song_name1)
        node2 = self.search(self.root, song_name2)
        if node1 is None or node2 is None:
            return None

        # Get the attribute value for each song
        attr1 = getattr(node1, attribute)
        attr2 = getattr(node2, attribute)

        comparison_result = attr1 - attr2  # Assuming the attribute is numeric and can be compared like this

        # Store the comparison result in the results stack
        self.results_stack.append((song_name1, song_name2, attribute, comparison_result))

        return comparison_result

    # Get method to retrieve the results stack
    def get_results_stack(self):
        return self.results_stack

# Define a Stack class for storing items
class Stack:
    def __init__(self):
        self.items = []

    # Method to push an item onto the stack
    def push(self, item):
        self.items.append(item)

    # Method to pop an item from the stack
    def pop(self):
        if not self.is_empty():
            return self.items.pop()

    # Method to check if the stack is empty
    def is_empty(self):
        return len(self.items) == 0

# Create an instance of the BST class
bst = BST()
popularity_stack = Stack()

# Insert songs into the BST using DataFrame columns
for index, row in df.iterrows():
    bst.insert(row['song_name'], row['album'], row['release_date'], row['acousticness'], row['danceability'], row['energy'], row['instrumentalness'], row['liveness'], row['loudness'], row['speechiness'], row['tempo'], row['valence'], row['popularity'])

# Create an instance of the Stack class
stack = Stack()

# Push songs into the popularity stack (sorted by popularity)
for index, row in df_sorted.iterrows():
    popularity_stack.push(row['song_name'])

# Display the contents of the popularity stack
print("Songs sorted by popularity:")
while not popularity_stack.is_empty():
    print(popularity_stack.pop())

# Ask the user to enter song names and attribute for comparison
song_name1 = input("Enter the first song name: ")
song_name2 = input("Enter the second song name: ")
attribute = input("Enter the attribute name to compare (e.g., energy, tempo, popularity): ")

# Perform comparison based on the specified attribute using the BST
result = bst.compare_attributes(song_name1, song_name2, attribute)
if result is not None:
    if result > 0:
        print(f"{song_name1} has higher {attribute} than {song_name2}.")
    elif result < 0:
        print(f"{song_name2} has higher {attribute} than {song_name1}.")
    else:
        print(f"{attribute} is equal between {song_name1} and {song_name2}.")
else:
    print("One or both songs not found in the BST.")

# Get the results stack and display its contents
results_stack = bst.get_results_stack()
print("Comparison results:")
for item in results_stack:
    print(item)
