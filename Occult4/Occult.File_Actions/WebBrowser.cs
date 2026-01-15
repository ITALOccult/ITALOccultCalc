using System;
using System.ComponentModel;
using System.Drawing;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult.File_Actions
{
	public class WebBrowser : Form
	{
		private IContainer components;

		private ToolStripButton toolStripPrevious;

		private ToolStripButton toolStripNext;

		private ToolStripButton toolStripRefresh;

		private ToolStripButton toolStripPrint;

		private WebBrowser webBrowser1;

		private StatusStrip statusStrip1;

		private ToolStripStatusLabel toolStripStatusLabel1;

		private ToolStripProgressBar toolStripProgressBar1;

		internal ToolStripComboBox toolStripComboBox1;

		internal ToolStrip toolStrip1;

		private ToolTip toolTip1;

		private ToolStripButton toolStripHome;

		public WebBrowser()
		{
			InitializeComponent();
		}

		private void WebBrowser_Load(object sender, EventArgs e)
		{
			((ToolStripItem)toolStripPrevious).set_Enabled(false);
			((ToolStripItem)toolStripHome).set_Enabled(false);
		}

		private void toolStripNext_Click(object sender, EventArgs e)
		{
			myBrowser();
		}

		internal void myBrowser()
		{
			//IL_0042: Unknown result type (might be due to invalid IL or missing references)
			//IL_004c: Expected O, but got Unknown
			//IL_0087: Unknown result type (might be due to invalid IL or missing references)
			//IL_0091: Expected O, but got Unknown
			//IL_009e: Unknown result type (might be due to invalid IL or missing references)
			//IL_00a8: Expected O, but got Unknown
			string text = "";
			if (((ToolStripItem)toolStripComboBox1).get_Text() != "")
			{
				text = ((ToolStripItem)toolStripComboBox1).get_Text();
			}
			webBrowser1.Navigate(text);
			webBrowser1.add_ProgressChanged(new WebBrowserProgressChangedEventHandler(webpage_ProgressChanged));
			webBrowser1.add_DocumentTitleChanged((EventHandler)webpage_DocumentTitleChanged);
			webBrowser1.add_StatusTextChanged((EventHandler)webpage_StatusTextChanged);
			webBrowser1.add_Navigated(new WebBrowserNavigatedEventHandler(webpage_Navigated));
			webBrowser1.add_DocumentCompleted(new WebBrowserDocumentCompletedEventHandler(webpage_DocumentCompleted));
		}

		private void webpage_DocumentCompleted(object sender, WebBrowserDocumentCompletedEventArgs e)
		{
			if (webBrowser1.get_CanGoBack())
			{
				((ToolStripItem)toolStripPrevious).set_Enabled(true);
			}
			else
			{
				((ToolStripItem)toolStripPrevious).set_Enabled(false);
			}
			if (webBrowser1.get_CanGoForward())
			{
				((ToolStripItem)toolStripHome).set_Enabled(true);
			}
			else
			{
				((ToolStripItem)toolStripHome).set_Enabled(false);
			}
			((ToolStripItem)toolStripStatusLabel1).set_Text("Done");
		}

		private void webpage_DocumentTitleChanged(object sender, EventArgs e)
		{
			((Control)this).set_Text(webBrowser1.get_DocumentTitle().ToString());
		}

		private void webpage_StatusTextChanged(object sender, EventArgs e)
		{
			((ToolStripItem)toolStripStatusLabel1).set_Text(webBrowser1.get_StatusText());
		}

		private void webpage_ProgressChanged(object sender, WebBrowserProgressChangedEventArgs e)
		{
			toolStripProgressBar1.set_Maximum((int)e.get_MaximumProgress());
			toolStripProgressBar1.set_Value((int)(((int)e.get_CurrentProgress() < 0 || (int)e.get_MaximumProgress() < (int)e.get_CurrentProgress()) ? e.get_MaximumProgress() : e.get_CurrentProgress()));
		}

		private void webpage_Navigated(object sender, WebBrowserNavigatedEventArgs e)
		{
			((ToolStripItem)toolStripComboBox1).set_Text(webBrowser1.get_Url().ToString());
		}

		private void toolStripPrevious_Click(object sender, EventArgs e)
		{
			webBrowser1.GoBack();
		}

		private void toolStripHome_Click(object sender, EventArgs e)
		{
			webBrowser1.GoHome();
		}

		private void toolStripPrint_Click(object sender, EventArgs e)
		{
			webBrowser1.ShowPrintPreviewDialog();
		}

		private void toolStripRefresh_Click(object sender, EventArgs e)
		{
			((Control)webBrowser1).Refresh();
		}

		private void toolStripComboBox1_KeyUp(object sender, KeyEventArgs e)
		{
			if (e.get_KeyValue() == 13)
			{
				myBrowser();
			}
		}

		protected override void Dispose(bool disposing)
		{
			if (disposing && components != null)
			{
				components.Dispose();
			}
			((Form)this).Dispose(disposing);
		}

		private void InitializeComponent()
		{
			//IL_001c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0026: Expected O, but got Unknown
			//IL_0027: Unknown result type (might be due to invalid IL or missing references)
			//IL_0031: Expected O, but got Unknown
			//IL_0032: Unknown result type (might be due to invalid IL or missing references)
			//IL_003c: Expected O, but got Unknown
			//IL_003d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0047: Expected O, but got Unknown
			//IL_0048: Unknown result type (might be due to invalid IL or missing references)
			//IL_0052: Expected O, but got Unknown
			//IL_0053: Unknown result type (might be due to invalid IL or missing references)
			//IL_005d: Expected O, but got Unknown
			//IL_0064: Unknown result type (might be due to invalid IL or missing references)
			//IL_006e: Expected O, but got Unknown
			//IL_006f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0079: Expected O, but got Unknown
			//IL_007a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0084: Expected O, but got Unknown
			//IL_0085: Unknown result type (might be due to invalid IL or missing references)
			//IL_008f: Expected O, but got Unknown
			//IL_0090: Unknown result type (might be due to invalid IL or missing references)
			//IL_009a: Expected O, but got Unknown
			//IL_009b: Unknown result type (might be due to invalid IL or missing references)
			//IL_00a5: Expected O, but got Unknown
			//IL_0196: Unknown result type (might be due to invalid IL or missing references)
			//IL_01a0: Expected O, but got Unknown
			components = new Container();
			ComponentResourceManager componentResourceManager = new ComponentResourceManager(typeof(WebBrowser));
			toolStrip1 = new ToolStrip();
			toolStripComboBox1 = new ToolStripComboBox();
			webBrowser1 = new WebBrowser();
			statusStrip1 = new StatusStrip();
			toolStripStatusLabel1 = new ToolStripStatusLabel();
			toolStripProgressBar1 = new ToolStripProgressBar();
			toolTip1 = new ToolTip(components);
			toolStripPrevious = new ToolStripButton();
			toolStripNext = new ToolStripButton();
			toolStripRefresh = new ToolStripButton();
			toolStripPrint = new ToolStripButton();
			toolStripHome = new ToolStripButton();
			((Control)toolStrip1).SuspendLayout();
			((Control)statusStrip1).SuspendLayout();
			((Control)this).SuspendLayout();
			toolStrip1.get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[6]
			{
				(ToolStripItem)toolStripPrevious,
				(ToolStripItem)toolStripNext,
				(ToolStripItem)toolStripRefresh,
				(ToolStripItem)toolStripHome,
				(ToolStripItem)toolStripComboBox1,
				(ToolStripItem)toolStripPrint
			});
			((Control)toolStrip1).set_Location(new Point(0, 0));
			((Control)toolStrip1).set_Name("toolStrip1");
			((Control)toolStrip1).set_Size(new Size(800, 25));
			((Control)toolStrip1).set_TabIndex(0);
			((Control)toolStrip1).set_Text("toolStrip1");
			((ToolStripItem)toolStripComboBox1).set_Name("toolStripComboBox1");
			((ToolStripItem)toolStripComboBox1).set_Size(new Size(500, 25));
			((ToolStripControlHost)toolStripComboBox1).add_KeyUp(new KeyEventHandler(toolStripComboBox1_KeyUp));
			((Control)webBrowser1).set_Dock((DockStyle)5);
			((Control)webBrowser1).set_Location(new Point(0, 25));
			((Control)webBrowser1).set_MinimumSize(new Size(20, 20));
			((Control)webBrowser1).set_Name("webBrowser1");
			((Control)webBrowser1).set_Size(new Size(800, 425));
			((Control)webBrowser1).set_TabIndex(1);
			((ToolStrip)statusStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[2]
			{
				(ToolStripItem)toolStripStatusLabel1,
				(ToolStripItem)toolStripProgressBar1
			});
			((Control)statusStrip1).set_Location(new Point(0, 428));
			((Control)statusStrip1).set_Name("statusStrip1");
			((Control)statusStrip1).set_Size(new Size(800, 22));
			((Control)statusStrip1).set_TabIndex(2);
			((Control)statusStrip1).set_Text("statusStrip1");
			((ToolStripItem)toolStripStatusLabel1).set_Name("toolStripStatusLabel1");
			((ToolStripItem)toolStripStatusLabel1).set_Size(new Size(0, 17));
			((ToolStripItem)toolStripProgressBar1).set_Name("toolStripProgressBar1");
			((ToolStripItem)toolStripProgressBar1).set_Size(new Size(100, 16));
			((ToolStripItem)toolStripPrevious).set_DisplayStyle((ToolStripItemDisplayStyle)2);
			((ToolStripItem)toolStripPrevious).set_Image((Image)Resources.arrow_Previous_16xLG_color);
			((ToolStripItem)toolStripPrevious).set_ImageTransparentColor(Color.Magenta);
			((ToolStripItem)toolStripPrevious).set_Name("toolStripPrevious");
			((ToolStripItem)toolStripPrevious).set_Size(new Size(23, 22));
			((ToolStripItem)toolStripPrevious).set_Text("Previous");
			((ToolStripItem)toolStripPrevious).set_ToolTipText("Click to go back");
			((ToolStripItem)toolStripPrevious).add_Click((EventHandler)toolStripPrevious_Click);
			((ToolStripItem)toolStripNext).set_DisplayStyle((ToolStripItemDisplayStyle)2);
			((ToolStripItem)toolStripNext).set_Image((Image)Resources.arrow_Next_16xLG_color);
			((ToolStripItem)toolStripNext).set_ImageTransparentColor(Color.Magenta);
			((ToolStripItem)toolStripNext).set_Name("toolStripNext");
			((ToolStripItem)toolStripNext).set_Size(new Size(23, 22));
			((ToolStripItem)toolStripNext).set_Text("toolStripButton3");
			((ToolStripItem)toolStripNext).set_ToolTipText("Click to go forward");
			((ToolStripItem)toolStripNext).add_Click((EventHandler)toolStripNext_Click);
			((ToolStripItem)toolStripRefresh).set_DisplayStyle((ToolStripItemDisplayStyle)2);
			((ToolStripItem)toolStripRefresh).set_Image((Image)Resources.Refresh_32x);
			((ToolStripItem)toolStripRefresh).set_ImageTransparentColor(Color.Magenta);
			((ToolStripItem)toolStripRefresh).set_Name("toolStripRefresh");
			((ToolStripItem)toolStripRefresh).set_Size(new Size(23, 22));
			((ToolStripItem)toolStripRefresh).set_Text("toolStripButton1");
			((ToolStripItem)toolStripRefresh).set_ToolTipText("Click to Refresh");
			((ToolStripItem)toolStripRefresh).add_Click((EventHandler)toolStripRefresh_Click);
			((ToolStripItem)toolStripPrint).set_Alignment((ToolStripItemAlignment)1);
			((ToolStripItem)toolStripPrint).set_DisplayStyle((ToolStripItemDisplayStyle)2);
			((ToolStripItem)toolStripPrint).set_Image((Image)Resources.printer);
			((ToolStripItem)toolStripPrint).set_ImageTransparentColor(Color.Magenta);
			((ToolStripItem)toolStripPrint).set_Name("toolStripPrint");
			((ToolStripItem)toolStripPrint).set_Size(new Size(23, 22));
			((ToolStripItem)toolStripPrint).set_Text("toolStripButton1");
			((ToolStripItem)toolStripPrint).set_ToolTipText("Print");
			((ToolStripItem)toolStripPrint).add_Click((EventHandler)toolStripPrint_Click);
			((ToolStripItem)toolStripHome).set_DisplayStyle((ToolStripItemDisplayStyle)2);
			((ToolStripItem)toolStripHome).set_Image((Image)Resources.Home);
			((ToolStripItem)toolStripHome).set_ImageTransparentColor(Color.Magenta);
			((ToolStripItem)toolStripHome).set_Name("toolStripHome");
			((ToolStripItem)toolStripHome).set_Size(new Size(23, 22));
			((ToolStripItem)toolStripHome).set_Text("toolStripButton2");
			((ToolStripItem)toolStripHome).add_Click((EventHandler)toolStripHome_Click);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(800, 450));
			((Control)this).get_Controls().Add((Control)(object)statusStrip1);
			((Control)this).get_Controls().Add((Control)(object)webBrowser1);
			((Control)this).get_Controls().Add((Control)(object)toolStrip1);
			((Form)this).set_Icon((Icon)componentResourceManager.GetObject("$this.Icon"));
			((Control)this).set_Name("WebBrowser");
			((Form)this).set_StartPosition((FormStartPosition)1);
			((Control)this).set_Text("WebBrowser");
			((Form)this).add_Load((EventHandler)WebBrowser_Load);
			((Control)toolStrip1).ResumeLayout(false);
			((Control)toolStrip1).PerformLayout();
			((Control)statusStrip1).ResumeLayout(false);
			((Control)statusStrip1).PerformLayout();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
