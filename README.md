# Yetは関数型のコンパイラ言語です

## 作成者 natsuakane

## 作成日　2023年8月30日

ファイル構成

　yet
 
　  src
   
　    yet_compiler.cpp 	Yet仮想コンパイラのソースコード
     
　    yet_VM.cpp       	Yet仮想環境のソースコード
     
　    yet_draw.cpp	画像生成のソースコード
     
　  mybin
   
　    yetc		yet_compiler.cppをg++でコンパイルしてできた実行ファイル
     
　    yet		yet_VM.cppをg++でコンパイルしてできた実行ファイル
     
　    yet_draw		yet_draw.cppをg++でコンパイルしてできた実行ファイル
     
　    yetd		オプション解析をしてyet_drawに渡すシェルスクリプト
     
　    visual_yet.html	Yetヴィジュアル環境
     
　  demo
   
　    ***.yet 		yet言語のプログラム

　    ***.ye		yet仮想環境で実行可能なyet実行ファイル
     
　    ***.lisp		rosで実行可能なlispのプログラム
     
　    ***.svg		生成された画像ファイル
     

以下のようにコマンドを実行してからコンパイル・実行をしてください

  1. zipファイルを展開し、yetディレクトリで端末を開いてください
  
    $ unzip yet.zip
    
  2. mybinにパスを通してください
  
    $ export PATH="$PATH:$(pwd)/mybin"
    
  3. yet言語のサンプルコードのあるディレクトリに移動してください
  
    $ cd demo
    
    
yet言語のコンパイル・実行方法（throw.yetの場合）

  1. コンパイル
  
    $ yetc -o throw.ye throw.yet
    
  2. 実行
  
    $ yet throw.ye
    
  
Yet言語のLisp言語へのトランスパイル・実行方法（throw.yetの場合）

  1. トランスパイル
  
    $ yetc -L -o throw.lisp throw.yet
    
  2. 実行（Lisp言語の実行環境roswellを事前に用意）
  
    $ ros throw.lisp
    

Yet言語の画像生成（testsvg.yetの場合）

  $ yetd -o test.svg testsvg.yet
  

Yetヴィジュアル環境での仮想機械語ファイルの実行方法（throw.yeの場合）

  1. FireFoxなどのJavaScriptが実行可能なブラウザでvisual_yet.htmlを開く。
  
  2. 参照ボタンを押しyet/demo/throw.yeを指定する。
  
  3. 手動実行ボタンまたは自動実行ボタンを押して実行する。自動実行の間隔は隣のスライダーで指定する。
  
  4. ターミナル部分に結果が表示される。
  

※別途PDFのマニュアル(yet_manual.pdf)がありますので参照してください
