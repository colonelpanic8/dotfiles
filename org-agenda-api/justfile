# org-agenda-api commands

base_url := "https://colonelpanic-org-agenda.fly.dev"
user := "imalison"

# Get all todos
get-all-todos:
    @curl -s -u "{{user}}:$(pass show org-agenda-api/imalison | head -1)" "{{base_url}}/get-all-todos" | jq .

# Get today's agenda
get-todays-agenda:
    @curl -s -u "{{user}}:$(pass show org-agenda-api/imalison | head -1)" "{{base_url}}/get-todays-agenda" | jq .

# Get agenda (day view)
agenda:
    @curl -s -u "{{user}}:$(pass show org-agenda-api/imalison | head -1)" "{{base_url}}/agenda" | jq .

# Get agenda files
agenda-files:
    @curl -s -u "{{user}}:$(pass show org-agenda-api/imalison | head -1)" "{{base_url}}/agenda-files" | jq .

# Get todo states
todo-states:
    @curl -s -u "{{user}}:$(pass show org-agenda-api/imalison | head -1)" "{{base_url}}/todo-states" | jq .

# Health check
health:
    @curl -s "{{base_url}}/health" | jq .

# Create a todo
create-todo title:
    @curl -s -X POST -u "{{user}}:$(pass show org-agenda-api/imalison | head -1)" \
        -H "Content-Type: application/json" \
        -d '{"title": "{{title}}"}' \
        "{{base_url}}/create-todo" | jq .
