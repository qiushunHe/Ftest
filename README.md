## FDA Challenge
<p style="color:#4d80e4;font-weight:bold"><u>(一) 涉及软件的安装和使用</u></p>

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
4. 提交及更新到git服务器
```
git add *
git commit -m "message"
## 第一次需要 -u 参数
git push -u origin master
```
5. 冲突解决
```
## 如果在更新出错，可能是合作者同时更改了文件并早于你提交，此时需要先同步，再更新
git pull origin master
## merge后
git push origin master
```

<p style="color:#4d80e4;font-weight:bold"><u>(二) 内容说明</u></p>