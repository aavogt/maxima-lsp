import subprocess
import sys

# The JSON body
body = '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"processId":12345,"rootUri":"rootori","capabilities":{}}}'
content_length = len(body.encode('utf-8'))

# Format the message
message = f"Content-Length: {content_length}\r\n\r\n{body}"

# Start maxima-lsp
proc = subprocess.Popen(
    ["maxima-lsp"],
    stdin=subprocess.PIPE,
    stdout=subprocess.PIPE,
    stderr=subprocess.PIPE
)

# Send the message
stdout, stderr = proc.communicate(input=message.encode('utf-8'))

# Print response
print("Response:")
print(stdout.decode('utf-8'))
print("Errors:")
print(stderr.decode('utf-8'))
