return {
    source = {
        type = 'dist',
        location = 'https://s3.amazonaws.com/json-c_releases/releases/json-c-0.12.tar.gz',
        sha256sum = '000c01b2b3f82dcb4261751eb71f1b084404fb7d6a282f06074d3c17078b9f3f'
    },
    build = {
        type = 'gnu',
        autoreconf = true
    }
}
