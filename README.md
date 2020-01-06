## FDA Challenge
``` 
涉及软件的安装和使用
```
### github使用
1. 安装
```
git config --global user.name "name"
git config --global user.email "email@example.com"
```
2. github配置
```
ssh-keygen -t rsa -C "email@example.com"
会生成/home/.ssh下id_ras(私钥)和id_rsa.pub，将此在github / Account settings / SSH Keys 添加id_rsa.pub
```
3. clone
```
git clone git@github.com:qiushunHe/Ftest.git 
```
4. 提交
```
git add *
git commit -m "message"
git push -u origin master
```