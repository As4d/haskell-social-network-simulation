# Haskell Social Network Simulation

A concurrent social network simulation where multiple user threads send messages to each other.

## Requirements

- Stack

## Building

Build the project:

```bash
stack build
```

## Running

Run the simulation:

```bash
stack run
```

The program will:
- Create 10 users
- Send 100 messages between users
- Display message counts for each user
- Export a timestamped message log to `message_log.txt`

## Documentation

Generate and open documentation:

```bash
stack haddock --open
```

Scroll down to `social-network-simulation-X.X.X.X` for the project-related documentation.

