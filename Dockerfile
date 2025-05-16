# 移除可能存在的隐藏文件
rm -f ._Dockerfile
rm -f Dockerfile

# 使用 cat 创建新的 Dockerfile
cat > Dockerfile << 'EOF'
FROM --platform=linux/amd64 ubuntu:20.04

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update && apt-get install -y \
    cmake \
    make \
    g++ \
    build-essential \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /src

CMD ["/bin/bash"]
EOF