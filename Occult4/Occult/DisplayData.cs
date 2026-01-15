using System;
using System.ComponentModel;
using System.Drawing;
using System.Windows.Forms;

namespace Occult
{
	public class DisplayData : Form
	{
		private Timer FlashTimer;

		private static readonly int ColorCount = 26;

		private static Color[] C = new Color[ColorCount];

		private static int CurrentColor = 0;

		internal int HelpTopic;

		private IContainer components;

		internal TextBox txtBox;

		private MenuStrip menuStrip1;

		private ToolStripMenuItem withTextToolStripMenuItem;

		private ToolStripMenuItem copySelectedToolStripMenuItem;

		private ToolStripMenuItem copyAllToolStripMenuItem;

		internal ToolStripMenuItem cmdOK;

		internal ToolStripMenuItem cmdCancel;

		internal ToolStripMenuItem helpToolStripMenuItem;

		public DisplayData()
		{
			//IL_000d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0017: Expected O, but got Unknown
			InitializeComponent();
			FlashTimer = new Timer();
			FlashTimer.set_Interval(150);
			FlashTimer.add_Tick((EventHandler)FlashTimer_Tick);
		}

		private void FlashTimer_Tick(object sender, EventArgs e)
		{
			CurrentColor++;
			if (CurrentColor >= ColorCount)
			{
				CurrentColor = 0;
			}
			((Control)txtBox).set_BackColor(C[CurrentColor]);
			Application.DoEvents();
		}

		public void NoColors()
		{
			FlashTimer.Stop();
			((Control)txtBox).set_BackColor(SystemColors.Window);
			((Control)txtBox).set_ForeColor(SystemColors.WindowText);
		}

		public void StartYellowRise()
		{
			for (int i = 0; i < 26; i++)
			{
				C[i] = Color.FromArgb(255, 255, 255, 255 - 9 * i);
			}
			((Control)txtBox).set_ForeColor(Color.Blue);
			FlashTimer.set_Interval(60);
			FlashTimer.Start();
		}

		public void StartBeigeRise()
		{
			for (int i = 0; i < 26; i++)
			{
				C[i] = Color.FromArgb(255, (int)(255.0 - 1.12 * (double)i), (int)(255.0 - 2.4 * (double)i), (int)(255.0 - 4.64 * (double)i));
			}
			((Control)txtBox).set_ForeColor(Color.Maroon);
			FlashTimer.set_Interval(60);
			FlashTimer.Start();
		}

		private void DisplayData_Resize(object sender, EventArgs e)
		{
			((Control)txtBox).set_Width(((Control)this).get_Width() - 26);
			((Control)txtBox).set_Height(((Control)this).get_Height() - 74);
		}

		private void cmdOK_Click(object sender, EventArgs e)
		{
		}

		private void cmdCancel_Click(object sender, EventArgs e)
		{
		}

		private void cmdCopy_Click(object sender, EventArgs e)
		{
			Clipboard.SetText(((Control)txtBox).get_Text());
		}

		private void DisplayData_Load(object sender, EventArgs e)
		{
			((Control)this).set_Text(((Control)this).get_Text() + " : Occult v." + Utilities.OccultVersion_Short);
		}

		private void copySelectedToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Clipboard.SetText(((TextBoxBase)txtBox).get_SelectedText());
		}

		private void copyAllToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Clipboard.SetText(((Control)txtBox).get_Text());
		}

		private void oKToolStripMenuItem_Click(object sender, EventArgs e)
		{
			((Form)this).set_DialogResult((DialogResult)1);
			((Form)this).Close();
			((Component)this).Dispose();
		}

		private void cancelToolStripMenuItem_Click(object sender, EventArgs e)
		{
			((Form)this).set_DialogResult((DialogResult)2);
			((Form)this).Close();
			((Component)this).Dispose();
		}

		private void helpToolStripMenuItem_Click(object sender, EventArgs e)
		{
			if (HelpTopic == 0)
			{
				Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Asteroid diameters");
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
			//IL_0001: Unknown result type (might be due to invalid IL or missing references)
			//IL_000b: Expected O, but got Unknown
			//IL_000c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0016: Expected O, but got Unknown
			//IL_0017: Unknown result type (might be due to invalid IL or missing references)
			//IL_0021: Expected O, but got Unknown
			//IL_0022: Unknown result type (might be due to invalid IL or missing references)
			//IL_002c: Expected O, but got Unknown
			//IL_002d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0037: Expected O, but got Unknown
			//IL_0038: Unknown result type (might be due to invalid IL or missing references)
			//IL_0042: Expected O, but got Unknown
			//IL_0043: Unknown result type (might be due to invalid IL or missing references)
			//IL_004d: Expected O, but got Unknown
			//IL_004e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0058: Expected O, but got Unknown
			txtBox = new TextBox();
			menuStrip1 = new MenuStrip();
			withTextToolStripMenuItem = new ToolStripMenuItem();
			copySelectedToolStripMenuItem = new ToolStripMenuItem();
			copyAllToolStripMenuItem = new ToolStripMenuItem();
			cmdOK = new ToolStripMenuItem();
			cmdCancel = new ToolStripMenuItem();
			helpToolStripMenuItem = new ToolStripMenuItem();
			((Control)menuStrip1).SuspendLayout();
			((Control)this).SuspendLayout();
			((Control)txtBox).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtBox).set_Location(new Point(6, 27));
			((TextBoxBase)txtBox).set_Multiline(true);
			((Control)txtBox).set_Name("txtBox");
			txtBox.set_ScrollBars((ScrollBars)3);
			((Control)txtBox).set_Size(new Size(408, 324));
			((Control)txtBox).set_TabIndex(2);
			((TextBoxBase)txtBox).set_WordWrap(false);
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[4]
			{
				(ToolStripItem)withTextToolStripMenuItem,
				(ToolStripItem)cmdOK,
				(ToolStripItem)cmdCancel,
				(ToolStripItem)helpToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(420, 24));
			((Control)menuStrip1).set_TabIndex(4);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripDropDownItem)withTextToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[2]
			{
				(ToolStripItem)copySelectedToolStripMenuItem,
				(ToolStripItem)copyAllToolStripMenuItem
			});
			((ToolStripItem)withTextToolStripMenuItem).set_Name("withTextToolStripMenuItem");
			((ToolStripItem)withTextToolStripMenuItem).set_Size(new Size(89, 20));
			((ToolStripItem)withTextToolStripMenuItem).set_Text("with text...     ");
			((ToolStripItem)copySelectedToolStripMenuItem).set_Name("copySelectedToolStripMenuItem");
			((ToolStripItem)copySelectedToolStripMenuItem).set_Size(new Size(148, 22));
			((ToolStripItem)copySelectedToolStripMenuItem).set_Text("Copy selected");
			((ToolStripItem)copySelectedToolStripMenuItem).add_Click((EventHandler)copySelectedToolStripMenuItem_Click);
			((ToolStripItem)copyAllToolStripMenuItem).set_Name("copyAllToolStripMenuItem");
			((ToolStripItem)copyAllToolStripMenuItem).set_Size(new Size(148, 22));
			((ToolStripItem)copyAllToolStripMenuItem).set_Text("Copy All");
			((ToolStripItem)copyAllToolStripMenuItem).add_Click((EventHandler)copyAllToolStripMenuItem_Click);
			((ToolStripItem)cmdOK).set_Font(new Font("Segoe UI", 9f, FontStyle.Bold));
			((ToolStripItem)cmdOK).set_Name("cmdOK");
			((ToolStripItem)cmdOK).set_Size(new Size(66, 20));
			((ToolStripItem)cmdOK).set_Text("Close      ");
			((ToolStripItem)cmdOK).add_Click((EventHandler)oKToolStripMenuItem_Click);
			((ToolStripItem)cmdCancel).set_Font(new Font("Segoe UI", 9f, FontStyle.Bold));
			((ToolStripItem)cmdCancel).set_Name("cmdCancel");
			((ToolStripItem)cmdCancel).set_Size(new Size(70, 20));
			((ToolStripItem)cmdCancel).set_Text("Cancel     ");
			((ToolStripItem)cmdCancel).add_Click((EventHandler)cancelToolStripMenuItem_Click);
			((ToolStripItem)helpToolStripMenuItem).set_BackColor(Color.Cyan);
			((ToolStripItem)helpToolStripMenuItem).set_Font(new Font("Segoe UI", 9f, FontStyle.Bold));
			((ToolStripItem)helpToolStripMenuItem).set_Name("helpToolStripMenuItem");
			((ToolStripItem)helpToolStripMenuItem).set_Size(new Size(45, 20));
			((ToolStripItem)helpToolStripMenuItem).set_Text("Help");
			((ToolStripItem)helpToolStripMenuItem).set_Visible(false);
			((ToolStripItem)helpToolStripMenuItem).add_Click((EventHandler)helpToolStripMenuItem_Click);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(420, 359));
			((Control)this).get_Controls().Add((Control)(object)txtBox);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Form)this).set_MaximizeBox(false);
			((Form)this).set_MinimizeBox(false);
			((Control)this).set_MinimumSize(new Size(200, 150));
			((Control)this).set_Name("DisplayData");
			((Control)this).set_Text("Display Data");
			((Form)this).add_Load((EventHandler)DisplayData_Load);
			((Control)this).add_Resize((EventHandler)DisplayData_Resize);
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
