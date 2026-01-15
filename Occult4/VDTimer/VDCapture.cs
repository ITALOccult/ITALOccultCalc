using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using System.Threading;
using System.Windows.Forms;

namespace VDTimer
{
	public static class VDCapture
	{
		private static IntPtr _hWnd = IntPtr.Zero;

		internal static bool UsingLiMovie = false;

		private static bool CapturingLiMovie = false;

		internal static bool UsingGStar = false;

		private static bool CapturingGStar = false;

		private static string GetTitleBarText(IntPtr hWnd)
		{
			StringBuilder stringBuilder = new StringBuilder(Win32.GetWindowTextLength(hWnd) + 1);
			Win32.GetWindowText(hWnd, stringBuilder, stringBuilder.Capacity);
			return stringBuilder.ToString();
		}

		private static IntPtr[] FindVirtualDubWindows()
		{
			List<IntPtr> list = new List<IntPtr>();
			IntPtr intPtr = IntPtr.Zero;
			while (true)
			{
				intPtr = Win32.FindWindowEx(IntPtr.Zero, intPtr, "VirtualDub", IntPtr.Zero);
				if (intPtr == IntPtr.Zero)
				{
					break;
				}
				list.Add(intPtr);
			}
			return list.ToArray();
		}

		private static IntPtr[] FindLiMovieWindows()
		{
			List<IntPtr> list = new List<IntPtr>();
			IntPtr intPtr = IntPtr.Zero;
			while (true)
			{
				intPtr = Win32.FindWindowEx(IntPtr.Zero, intPtr, "TCapture_1", IntPtr.Zero);
				if (intPtr == IntPtr.Zero)
				{
					break;
				}
				list.Add(intPtr);
			}
			return list.ToArray();
		}

		private static IntPtr[] FindGStarWindows()
		{
			List<IntPtr> list = new List<IntPtr>();
			IntPtr intPtr = IntPtr.Zero;
			while (true)
			{
				intPtr = Win32.FindWindowEx(IntPtr.Zero, intPtr, "ThunderRT6MDIForm", IntPtr.Zero);
				if (intPtr == IntPtr.Zero)
				{
					break;
				}
				list.Add(intPtr);
			}
			return list.ToArray();
		}

		private static bool IsInCaptureMode(IntPtr hWnd)
		{
			if (UsingLiMovie)
			{
				return CapturingLiMovie;
			}
			if (UsingGStar)
			{
				return CapturingGStar;
			}
			return GetTitleBarText(hWnd).IndexOf(" - capture mode [") != -1;
		}

		public static bool FindCaptureWindow()
		{
			IntPtr[] array = FindVirtualDubWindows();
			if (array.Length != 0)
			{
				IntPtr intPtr = IntPtr.Zero;
				IntPtr[] array2 = array;
				foreach (IntPtr intPtr2 in array2)
				{
					if (IsInCaptureMode(intPtr2) && GetCapturePath(intPtr2).Length != 0)
					{
						if (!(intPtr == IntPtr.Zero))
						{
							intPtr = IntPtr.Zero;
							break;
						}
						intPtr = intPtr2;
					}
				}
				_hWnd = intPtr;
				UsingLiMovie = false;
				UsingGStar = false;
				return _hWnd != IntPtr.Zero;
			}
			array = FindLiMovieWindows();
			if (array.Length != 0)
			{
				IntPtr intPtr3 = IntPtr.Zero;
				IntPtr[] array2 = array;
				foreach (IntPtr intPtr4 in array2)
				{
					if (intPtr3 == IntPtr.Zero)
					{
						intPtr3 = intPtr4;
						continue;
					}
					intPtr3 = IntPtr.Zero;
					break;
				}
				_hWnd = intPtr3;
				UsingLiMovie = true;
				UsingGStar = false;
				return _hWnd != IntPtr.Zero;
			}
			array = FindGStarWindows();
			if (array.Length != 0)
			{
				IntPtr intPtr5 = IntPtr.Zero;
				IntPtr[] array2 = array;
				foreach (IntPtr intPtr6 in array2)
				{
					if (intPtr5 == IntPtr.Zero)
					{
						intPtr5 = intPtr6;
						continue;
					}
					intPtr5 = IntPtr.Zero;
					break;
				}
				_hWnd = intPtr5;
				UsingLiMovie = false;
				UsingGStar = true;
				return _hWnd != IntPtr.Zero;
			}
			return false;
		}

		private static void VerifyCaptureWindow()
		{
			//IL_0021: Unknown result type (might be due to invalid IL or missing references)
			if (!UsingLiMovie && !UsingGStar && !IsInCaptureMode(_hWnd))
			{
				MessageBox.Show("Cannot find the capture window.");
			}
		}

		private static void FocusWindow(IntPtr hWnd)
		{
			if (Win32.GetForegroundWindow() != hWnd)
			{
				DateTime dateTime = DateTime.Now.AddSeconds(1.0);
				do
				{
					_ = DateTime.Now >= dateTime;
					Win32.SwitchToThisWindow(hWnd, fAltTab: false);
					Thread.Sleep(100);
				}
				while (Win32.GetForegroundWindow() != hWnd);
			}
		}

		private static void SendStartKeyStroke(IntPtr hWnd)
		{
			if (UsingLiMovie)
			{
				SendKeys.SendWait("{F1}");
			}
			else if (UsingGStar)
			{
				SendKeys.SendWait("%t");
			}
			else
			{
				SendKeys.SendWait("{F6}");
			}
		}

		private static void SendStopKeyStroke(IntPtr hWnd)
		{
			if (UsingGStar)
			{
				SendKeys.SendWait("%s");
			}
			else
			{
				SendKeys.SendWait("{ESC}");
			}
		}

		private static void KeyEsc()
		{
			Win32.keybd_event(27, 0, 0u, IntPtr.Zero);
			Win32.keybd_event(27, 0, 2u, IntPtr.Zero);
		}

		public static void StartCapture()
		{
			//IL_006b: Unknown result type (might be due to invalid IL or missing references)
			FocusWindow(_hWnd);
			CapturingLiMovie = false;
			if (UsingLiMovie)
			{
				SendStartKeyStroke(_hWnd);
				CapturingLiMovie = true;
				return;
			}
			if (UsingGStar)
			{
				SendStartKeyStroke(_hWnd);
				CapturingGStar = true;
				return;
			}
			DateTime dateTime = DateTime.Now.AddSeconds(10.0);
			while (!IsCapturing())
			{
				if (DateTime.Now >= dateTime)
				{
					MessageBox.Show("Unable to start video capture.");
				}
				SendStartKeyStroke(_hWnd);
				Thread.Sleep(800);
			}
		}

		public static void StopCapture()
		{
			//IL_003b: Unknown result type (might be due to invalid IL or missing references)
			//IL_00a2: Unknown result type (might be due to invalid IL or missing references)
			string capturePath = GetCapturePath();
			FocusWindow(_hWnd);
			DateTime dateTime = DateTime.Now.AddSeconds(10.0);
			while (IsCapturing())
			{
				if (DateTime.Now >= dateTime)
				{
					MessageBox.Show("Unable to stop video capture.");
				}
				FocusWindow(_hWnd);
				SendStopKeyStroke(_hWnd);
				KeyEsc();
				Thread.Sleep(800);
				CapturingLiMovie = false;
				CapturingGStar = false;
			}
			dateTime = DateTime.Now.AddSeconds(10.0);
			while (IsFileLocked(capturePath))
			{
				if (DateTime.Now >= dateTime)
				{
					MessageBox.Show("The lock on the capture file is not being released.");
				}
				Thread.Sleep(800);
			}
		}

		public static bool IsCapturing()
		{
			VerifyCaptureWindow();
			if (UsingLiMovie)
			{
				return CapturingLiMovie;
			}
			if (UsingGStar)
			{
				return CapturingGStar;
			}
			return GetTitleBarText(_hWnd).EndsWith(" [capture in progress]");
		}

		private static bool IsFileLocked(string path)
		{
			//IL_002c: Unknown result type (might be due to invalid IL or missing references)
			if (path == "")
			{
				return false;
			}
			FileStream fileStream = null;
			try
			{
				fileStream = new FileStream(path, FileMode.Open, FileAccess.ReadWrite, FileShare.None);
				return false;
			}
			catch (IOException)
			{
				return true;
			}
			catch
			{
				bool result = false;
				MessageBox.Show("Unable to check if file is locked.");
				return result;
			}
			finally
			{
				fileStream?.Close();
			}
		}

		private static string GetCapturePath(IntPtr hWnd)
		{
			string titleBarText = GetTitleBarText(hWnd);
			int num = titleBarText.IndexOf(" - capture mode [");
			if (num != -1)
			{
				num += " - capture mode [".Length;
				int num2 = titleBarText.IndexOf("]", num);
				if (num2 != -1)
				{
					return titleBarText.Substring(num, num2 - num);
				}
			}
			return string.Empty;
		}

		public static string GetCapturePath()
		{
			//IL_0036: Unknown result type (might be due to invalid IL or missing references)
			if (UsingLiMovie)
			{
				return "";
			}
			if (UsingGStar)
			{
				return "";
			}
			VerifyCaptureWindow();
			string capturePath = GetCapturePath(_hWnd);
			if (capturePath.Length == 0)
			{
				MessageBox.Show("Unable to find the path of the capture file.");
			}
			return capturePath;
		}
	}
}
