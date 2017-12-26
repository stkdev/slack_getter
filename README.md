# Slackからメッセージとってきて集計するツール
## 概要
タイトルのままです。バックアップや簡易的な集計にご利用ください。

※色々と失敗している感があるのでv1.0になるとガラッと変わっている可能性あり。

![screenshot](https://github.com/stkdev/slack_getter/blob/images/screenshot.png?raw=true)

## 機能

指定のSlackチャンネルのメッセージ履歴を取ってきて、一覧化＆可視化するツールです。

* ◯◯について書いてあるログを抽出する
* 整形してバックアップに使う
* 人ごとの発言回数をグラフ化する

とかに使える想定です。

## 使い方

### 設定

※すでに設定されている場合は不要
* Slackのトークンを取得します　[Slack API Legacy tokens](https://api.slack.com/custom-integrations/legacy-tokens)
* チャンネルIDを取得します
    * Web版のSlackで該当チャンネルを開いてURL見るのが楽です。
* 配布用URL
    * トークンとチャンネルIDをURLに引っ付けるとデフォルト値として利用できます。人に教える際にどうぞ。

### 仕様
* 取得範囲を選択
    * from側の00:00〜to側の00:00までの範囲となります
    * 例）「2017-12-25 〜 2017-12-27」と設定すると25,26日の48時間分が取得できます
    * デフォルト値は前日1日です
* 絞込ワード
    * 該当ワードがメッセージ本文内にあるものが取得できます
* カスタムフィルタ
    * 取得するSlackメッセージが特殊な場合（階層構造持たして書き込んでいる、とか）にオンにすると、処理規則に従ってメッセージを加工してから取得します。（※要開発）
    * なんか作りもUIもセンスがないのでどうにかしたい。
* グラフ
    * グラフはuserごとの件数をグラフ化しています。
    * もっと色々集計できるようにしたい。
* 制約
    * 一度にとってこれるメッセージは1000件までです。（SlackAPIの制限）

## 技術的なあれこれ

* R言語、Shinyフレームワークを使用してます
* なぜ配布のめんどくさいShinyにしたのか
    * → 反省しております。
    * Shiny Serverをご構築ください
